#lang racket/base

;; This contains the implementation for derived sequence functions that have no need to access the
;; internal representation of the underlying interfaces.

(require racket/require
         (multi-in data/collection [collection contract countable])
         (multi-in racket [contract generator generic stream])
         (prefix-in b: racket/list)
         match-plus)

(provide
 (rename-out [in-naturals naturals]
             [in-range range])
 (contract-out
  [for-each (->i ([proc (seqs) (and/c (procedure-arity-includes/c (length seqs))
                                      (unconstrained-domain-> any/c))])
                 #:rest [seqs (non-empty-listof sequence?)]
                 [result void?])]
  [foldl/steps (->i ([proc (seqs) (and/c (procedure-arity-includes/c (add1 (length seqs)))
                                         (unconstrained-domain-> any/c))]
                     [init any/c])
                    #:rest [seqs (non-empty-listof sequence?)]
                    [result sequence?])]
  [andmap (->i ([proc (seqs) (and/c (procedure-arity-includes/c (length seqs))
                                    (unconstrained-domain-> any/c))])
               #:rest [seqs (non-empty-listof sequence?)]
               [result any/c])]
  [ormap (->i ([proc (seqs) (and/c (procedure-arity-includes/c (length seqs))
                                   (unconstrained-domain-> any/c))])
              #:rest [seqs (non-empty-listof sequence?)]
              [result any/c])]
  [append-map (->i ([proc (seqs) (and/c (procedure-arity-includes/c (length seqs))
                                        (unconstrained-domain-> sequence?))])
                   #:rest [seqs (non-empty-listof sequence?)]
                   [result sequence?])]
  [find-best (->* [(and/c sequence? (not/c empty?)) (any/c any/c . -> . any/c)]
                  [#:key (any/c . -> . any/c)]
                  any/c)]
  [find-min (->* [(and/c sequence? (not/c empty?))] [#:key (any/c . -> . real?)] any/c)]
  [find-max (->* [(and/c sequence? (not/c empty?))] [#:key (any/c . -> . real?)] any/c)]
  [last ((and/c sequence? (not/c empty?)) . -> . any)]
  [index-of ([sequence? any/c] [(any/c any/c . -> . any/c)]
                               . ->* . (or/c exact-nonnegative-integer? #f))]
  [index-where (sequence? (any/c . -> . any/c) . -> . (or/c exact-nonnegative-integer? #f))]
  [build-sequence (case-> ((exact-nonnegative-integer? . -> . any/c) . -> . sequence?)
                          (exact-nonnegative-integer? (exact-nonnegative-integer? . -> . any/c)
                                                      . -> . sequence?))]
  [repeat (any/c . -> . sequence?)]
  [cycle ((and/c sequence? (not/c empty?)) . -> . sequence?)]
  [take (exact-nonnegative-integer? sequence? . -> . sequence?)]
  [drop (exact-nonnegative-integer? sequence? . -> . sequence?)]
  [subsequence (->i ([seq sequence?]
                     [start exact-nonnegative-integer?]
                     [end (start) (and/c exact-nonnegative-integer? (>=/c start))])
                    [result sequence?])]
  [subsequence* (sequence? exact-nonnegative-integer? exact-nonnegative-integer? . -> . sequence?)]
  [append* ([] #:rest (non-empty-listof sequence?) . ->* . sequence?)]
  [flatten (sequence? . -> . sequence?)]
  [indexed (sequence? . -> . sequence?)]
  [chunk (exact-nonnegative-integer? sequence? . -> . sequence?)]
  [chunk* (exact-nonnegative-integer? sequence? . -> . sequence?)]
  [cartesian-product ([] #:rest (listof sequence?) . ->* . (sequenceof sequence?))]
  [generate-sequence (generator? . -> . sequence?)]
  [sequence->string ((sequenceof char?) . -> . (and/c string? sequence?))]
  [sequence->bytes ((sequenceof byte?) . -> . (and/c bytes? sequence?))]
  [randoms (case-> (-> sequence?)
                   (-> (or/c (integer-in 1 4294967087) pseudo-random-generator?) sequence?)
                   (-> (integer-in 1 4294967087) pseudo-random-generator? sequence?))]))

; like map, but strict, returns void, and is only for side-effects
(define for-each
  (case-lambda
    [(proc seq)
     (let loop ([seq* seq])
       (cond
         [(empty? seq*) (void)]
         [else
          (proc (first seq*))
          (loop (rest seq*))]))]
    [(proc . seqs)
     (let loop ([seqs* seqs])
       (cond
         [(andmap empty? seqs*) (void)]
         [(ormap empty? seqs*)
          (raise-arguments-error
           'for-each "all sequences must have the same length"
           "proc" proc
           "sequences" seqs)]
         [else
          (apply proc (map first seqs*))
          (loop (map rest seqs*))]))]))

; like foldl, but lazily produces each step of the reduction
(define foldl/steps
  (case-lambda
    [(proc init seq)
     (let loop ([init* init]
                [seq* seq])
       (stream-cons init*
                    (if (empty? seq*)
                        empty-stream
                        (loop (proc init* (first seq*)) (rest seq*)))))]
    [(proc init . seqs)
     (let loop ([init* init]
                [seqs* seqs])
       (stream-cons init*
                    (cond
                      [(andmap empty? seqs*) empty-stream]
                      [(ormap empty? seqs*)
                       (raise-arguments-error
                        'foldl/steps "all sequences must have the same length"
                        "proc" proc
                        "init" init
                        "sequences" seqs)]
                      [else (loop (apply proc init* (map first seqs*)) (map rest seqs*))])))]))

; boolean folds for arbitrary sequences
(define (andmap proc . seqs)
  (apply foldl (λ (acc . vals) (and acc (apply proc vals))) #t seqs))
(define (ormap proc . seqs)
  (apply foldl (λ (acc . vals) (or acc (apply proc vals))) #f seqs))

; get an element that optimizes a given criterion
(define (find-best seq >? #:key [extract-key values])
  (define-values [v x]
    (for/fold ([v (first seq)]
               [x (extract-key (first seq))])
              ([v2 (in (rest seq))])
      (define x2 (extract-key v2))
      (if (>? x2 x)
          (values v2 x2)
          (values v x))))
  v)

; common cases of find-best
(define (find-min seq #:key [extract-key values])
  (find-best seq < #:key extract-key))
(define (find-max seq #:key [extract-key values])
  (find-best seq > #:key extract-key))

; get the end of a finite sequence
(define (last seq)
  (if (and (countable? seq)
           (known-finite? seq))
      (nth seq (sub1 (length seq)))
      (let loop ([seq seq])
        (let ([next (rest seq)])
          (if (empty? next)
              (first seq)
              (loop next))))))

; index-searching functions for sequences
(define (index-of seq x [=? equal?])
  (for/or ([y (in seq)]
           [i (in-naturals)])
    (and (=? y x) i)))
(define (index-where seq proc)
  (for/or ([y (in seq)]
           [i (in-naturals)])
    (and (proc y) i)))

; indexed sequence constructor
(define build-sequence
  (case-lambda
    [(proc)
     (let loop ([i 0])
       (stream-cons (proc i) (loop (add1 i))))]
    [(n proc)
     (let loop ([i 0])
       (if (= i n) empty-stream
           (stream-cons (proc i) (loop (add1 i)))))]))

; wrapper for ‘repeat’
(struct single-value-seq (val)
  #:reflection-name 'infinite-sequence
  #:methods gen:custom-write
  [(define (write-proc s out mode)
     (fprintf out "#<repeated-sequence:~a>" (single-value-seq-val s)))]
  #:methods gen:sequence
  [(define (empty? s) #f)
   (define (first s) (single-value-seq-val s))
   ; return a new value to unwrap any contracts so they don't build up
   (define (rest s) (single-value-seq (single-value-seq-val s)))
   (define (nth s i) (first s))
   (define (reverse s) (rest s))
   (define (random-access? s) #t)])

; infinite, single-valued sequence constructor
(define (repeat v)
  (single-value-seq v))

; wrapper for ‘cycle’
(struct cycled-seq (head current)
  #:reflection-name 'cycled-sequence
  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define (empty? s) #f)
   (define (first s) (-first (cycled-seq-current s)))
   (define/match* (rest (cycled-seq head current))
     (define current* (-rest current))
     (if (-empty? current*)
         (cycled-seq head head)
         (cycled-seq head current*)))])

; infinite, multi-valued sequence constructor
(define (cycle s)
  (cycled-seq s s))

; wrapper for lazy sections of a sequence
(struct bounded-seq (source left)
  #:reflection-name 'lazy-sequence
  #:methods gen:countable
  [(define/match* (length (bounded-seq _ left)) left)
   (define (known-finite seq) #t)]
  #:methods gen:sequence
  [(define/generic -first first)
   (define/generic -rest rest)
   (define/generic -nth nth)
   (define/match* (empty? (bounded-seq _ left))
     (zero? left))
   (define/match* (first (bounded-seq source _))
     (-first source))
   (define/match* (rest (bounded-seq source left))
     (bounded-seq (-rest source) (sub1 left)))
   (define/match* (nth (bounded-seq source _) index)
     (-nth source index))
   ; reversing the sequence can't possibly be lazy, anyway, so just turn it into a list
   (define/match* (reverse seq)
     (extend '() seq))])

; lazily grabs the first n elements of seq
(define (take n seq)
  (when (and (countable? seq)
             (known-finite? seq)
             (> n (length seq)))
    (raise-range-error 'take "sequence" "length " n seq 0 (length seq)))
  (bounded-seq seq n))

; strictly drops the first n elements of seq
(define (drop n seq)
  (when (and (countable? seq)
             (known-finite? seq)
             (> n (length seq)))
    (raise-range-error 'drop "sequence" "length " n seq 0 (length seq)))
  (let loop ([n n]
             [seq seq])
    (if (zero? n)
        seq
        (loop (sub1 n) (rest seq)))))

; utility for composing take and drop
(define (subsequence seq start end)
  (when (and (countable? seq)
             (known-finite? seq))
    (when (> start (length seq))
      (raise-range-error 'subsequence "sequence" "start " start seq 0 (length seq)))
    (when (> end (length seq))
      (raise-range-error 'subsequence "sequence" "end " end seq 0 (length seq))))
  (take (- end start) (drop start seq)))

; like subsequence but specifying a length instead of an end index
(define (subsequence* seq start len)
  (when (and (countable? seq)
             (known-finite? seq))
    (when (> start (length seq))
      (raise-range-error 'subsequence* "sequence" "start " start seq 0 (length seq)))
    (when (> (+ start len) (length seq))
      (raise-range-error 'subsequence* "sequence" "end " (+ start len) seq 0 (length seq))))
  (take len (drop start seq)))

; lazily flatten a sequence
(define (flatten seq)
  (generate-sequence
   (generator ()
     (let loop ([seq seq])
       (unless (empty? seq)
         (let ([head (first seq)]
               [tail (rest seq)])
           (if (sequence? head)
               (loop head)
               (yield head))
           (loop tail)))))))

; like (apply append seqs) but can be lazier
(define append*
  (case-lambda
    ; provide a fast case when only one argument is supplied
    [(seqs)
     (for*/sequence ([seq (in seqs)]
                     [e (in seq)])
       e)]
    ; use ‘append’ otherwise
    [seqs
     (define-values (init last) (b:split-at-right seqs 1))
     (append
      (apply append init)
      (for*/sequence ([seq (in (car last))]
                      [e (in seq)])
        e))]))

; like (apply append (map proc . seqs)) but lazier and less expensive
(define (append-map proc . seqs)
  (generate-sequence
   (generator ()
     (let loop ([seqs* seqs])
       (cond
         [(andmap empty? seqs*) (void)]
         [(ormap empty? seqs*)
          (raise-arguments-error
           'append-map "all sequences must have the same length"
           "proc" proc
           "sequences" seqs)]
         [else
          (for-each yield (apply proc (map first seqs*)))
          (loop (map rest seqs*))])))))

; maps over a sequence and creates pairs of each element and its index in the sequence
(define (indexed seq)
  (for/sequence ([x (in seq)]
                 [i (in-naturals)])
    (cons i x)))

; groups sequences into subsequences of length ‘n’
(define (chunk n seq)
  (if (empty? seq) empty-stream
      (let loop ([x n]
                 [seq seq]
                 [acc empty-stream])
        (if (or (zero? x) (empty? seq))
            (stream-cons (reverse acc) (chunk n seq))
            (loop (sub1 x) (rest seq) (stream-cons (first seq) acc))))))

; like ‘chunk’, but throws an exception if the sequence cannot be divided perfectly
(define (chunk* n seq)
  (if (empty? seq) empty-stream
      (let ([head (take n seq)]
            [tail (drop n seq)])
        (stream-cons head (chunk* n tail)))))

; performs a lazy, n-dimensional cartesian product
(define (cartesian-product . seqs)
  (define (cp-2 as bs)
    (for*/sequence ([i (in as)]
                    [j (in bs)])
      (stream-cons i j)))
  (foldr cp-2 (list empty-stream) seqs))

; creates a sequence by lazily pulling values from a generator
(define (generate-sequence g)
  (let loop ()
    (define v (g))
    (if (eq? 'done (generator-state g))
        empty-stream
        (stream-cons v (loop)))))

; some conversion functions for non-collections
(define (sequence->string seq)
  (string->immutable-string (list->string (sequence->list seq))))
(define (sequence->bytes seq)
  (bytes->immutable-bytes (list->bytes (sequence->list seq))))

; infinite sequence of random numbers
(define (randoms [k #f] [gen #f])
  (define integral? (integer? k))
  ; if ‘k’ isn't provided as an integer, it's actually the generator due to how the
  ; arity of this function works
  (define gen* (if integral? gen k))
  (define random-generator (or gen* (make-pseudo-random-generator)))
  (generate-sequence
   (generator ()
     (let loop ()
       (yield
        (if integral?
            (random k random-generator)
            (random random-generator)))
       (loop)))))
