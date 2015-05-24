#lang racket/base

(require (for-syntax racket/base)
         racket/lazy-require
         racket/generic
         racket/contract
         racket/generator
         racket/function
         unstable/function
         alexis/util/match
         alexis/util/renamed
         (prefix-in
          b: (combine-in
              racket/base
              racket/list
              racket/vector
              racket/set
              racket/stream
              racket/dict))
         "countable.rkt")

(lazy-require
 ["private/random-access.rkt" [sequence->random-access-sequence]])

(provide
 gen:collection collection? collection/c
 gen:sequence sequence? sequence/c
 apply in for/sequence for*/sequence
 (contract-out
  ; gen:collection
  [extend (collection? sequence? . -> . collection?)]
  [conj (collection? any/c . -> . collection?)]
  ; gen:sequence
  [empty? (sequence? . -> . boolean?)]
  [first ((and/c sequence? (not/c empty?)) . -> . any)]
  [rest ((and/c sequence? (not/c empty?)) . -> . any)]
  [rename nth* nth (sequence? exact-nonnegative-integer? . -> . any)]
  [reverse (sequence? . -> . sequence?)]
  ; derived functions
  [extend* ([collection?] #:rest (listof sequence?) . ->* . sequence?)]
  [conj* ([collection?] #:rest any/c . ->* . sequence?)]
  [append ([] #:rest (listof sequence?) . ->* . sequence?)]
  [filter ((any/c . -> . any/c) sequence? . -> . sequence?)]
  [map (->i ([proc (seqs) (and/c (procedure-arity-includes/c (b:length seqs))
                                 (unconstrained-domain-> any/c))])
            #:rest [seqs (non-empty-listof sequence?)]
            [result sequence?])]
  [foldl (->i ([proc (seqs) (and/c (procedure-arity-includes/c (add1 (b:length seqs)))
                                   (unconstrained-domain-> any/c))]
               [init any/c])
              #:rest [seqs (non-empty-listof sequence?)]
              [result any/c])]
  [sequence->list (sequence? . -> . list?)]
  ; helpers
  [second (sequence? . -> . any)]
  [third (sequence? . -> . any)]
  [fourth (sequence? . -> . any)]
  [fifth (sequence? . -> . any)]
  [sixth (sequence? . -> . any)]
  [seventh (sequence? . -> . any)]
  [eighth (sequence? . -> . any)]
  [ninth (sequence? . -> . any)]
  [tenth (sequence? . -> . any)]))

;; wrappers
;; ---------------------------------------------------------------------------------------------------

; provide nice range errors for countable sequences
(define (nth* seq i)
  (when (and (countable? seq)
             (known-finite? seq)
             (>= i (length seq)))
    (raise-range-error 'nth "sequence" "" i seq 0 (sub1 (length seq))))
  (nth seq i))

;; fallbacks
;; ---------------------------------------------------------------------------------------------------

(define (-conj coll item)
  (extend coll (list item)))

(define (-extend coll coll*)
  (foldl conj coll coll*))

(define (-empty? seq)
  (zero? (length seq)))

(define (-first seq)
  (nth seq 0))

(define (-nth seq index)
  (if (zero? index)
      (first seq)
      (nth (rest seq) (sub1 index))))

(define (stream-reverse str)
  (for/fold ([str* b:empty-stream])
            ([e (b:in-stream str)])
    (b:stream-cons e str*)))

;; generic interfaces
;; ---------------------------------------------------------------------------------------------------

; a collection is anything that can hold values
(define-generics collection
  (conj collection item)
  (extend collection collection*)
  #:fallbacks
  [(define conj -conj)
   (define extend -extend)]
  #:defaults
  ([list?
    (define (conj lst item) (cons item lst))]
   [(conjoin vector? immutable?)
    (define (conj vec item)
      (b:vector->immutable-vector
       (b:vector-append vec (b:vector item))))]
   [(conjoin hash? immutable?)
    (define/contract (conj hsh item)
      ((and/c hash? immutable?) pair? . -> . (and/c hash? immutable?))
      (hash-set hsh (car item) (cdr item)))]
   [b:set?
    (define conj b:set-add)]
   [(conjoin b:dict? (negate b:dict-mutable?) b:dict-can-functional-set?)
    (define/contract (conj dct item)
      ((and/c b:dict? (negate b:dict-mutable?) b:dict-can-functional-set?)
       pair? . -> . (and/c b:dict? (negate b:dict-mutable?) b:dict-can-functional-set?))
      (b:dict-set dct (car item) (cdr item)))]))

; a sequence is an ordered set of values
(define-generics sequence
  (empty? sequence)
  (first sequence)
  (rest sequence)
  (nth sequence index)
  (reverse sequence)
  #:fallbacks
  [(define empty? -empty?)
   (define first -first)
   (define nth -nth)]
  #:fast-defaults
  ([(disjoin pair? null?)
    (define empty? b:null?)
    (define first b:car)
    (define rest b:cdr)
    (define reverse b:reverse)])
  #:defaults
  ([(conjoin vector? immutable?)
    (define/generic -rest rest)
    (define/generic -reverse reverse)
    (define nth vector-ref)
    (define rest (compose1 -rest sequence->random-access-sequence))
    (define reverse (compose1 -reverse sequence->random-access-sequence))]
   [b:stream?
    (define empty? b:stream-empty?)
    (define first b:stream-first)
    (define rest b:stream-rest)
    (define reverse stream-reverse)]
   [(conjoin hash? immutable?)
    (define empty? hash-empty?)
    (define hash->stream (compose1 b:sequence->stream b:in-hash-pairs))
    (define first (compose1 b:stream-first hash->stream))
    (define rest (compose1 b:stream-rest hash->stream))
    (define reverse (compose1 stream-reverse hash->stream))]
   [b:set?
    (define empty? b:set-empty?)
    (define first (compose1 b:stream-first b:set->stream))
    (define rest (compose1 b:stream-rest b:set->stream))
    (define reverse (compose1 stream-reverse b:set->stream))]
   [(conjoin b:dict? (negate b:dict-mutable?))
    (define empty? b:dict-empty?)
    (define first (compose1 b:stream-first b:sequence->stream b:in-dict-pairs))
    (define rest (compose1 b:stream-rest b:sequence->stream b:in-dict-pairs))
    (define reverse (compose1 stream-reverse b:sequence->stream b:in-dict-pairs))]))

;; derived functions
;; ---------------------------------------------------------------------------------------------------

; two sequences concatenated together in order, for laziness
(struct concatenated-sequence (a b)
  #:reflection-name 'lazy-sequence
  #:methods gen:countable
  [(define/generic -length length)
   (define/match* (length (concatenated-sequence a b))
     (+ (-length a) (-length b)))]
  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/match* (empty? (concatenated-sequence a b))
     (and (-empty? a) (-empty? b)))
   (define/match* (first (concatenated-sequence a b))
     (if (-empty? a) (-first b) (-first a)))
   (define/match* (rest (concatenated-sequence a b))
     (if (-empty? a) (-rest b) (concatenated-sequence (-rest a) b)))])

; validates the arguments passed to apply and converts the final argument to a list from a sequence
(define (parse-apply-arguments args)
  (define fn (first args))
  (when (not (procedure? fn))
    (b:apply raise-argument-error 'apply "procedure?" 0 args))
  (define main-args (rest args))
  (define-values (init last) (b:split-at main-args (sub1 (length main-args))))
  (when (not (sequence? (b:first last)))
    (b:apply raise-argument-error 'apply "sequence?" (sub1 (length main-args)) args))
  (define last* (reverse (extend '() (b:first last))))
  (values fn (b:append init (list last*))))

; implementation of apply when no keyword arguments are supplied
(define/renamed apply (apply/basic . args)
  (when (b:empty? args)
    (b:apply raise-arity-error 'apply (arity-at-least 1) args))
  (if (= (b:length args) 1)
      ((b:first args))
      (let-values ([(fn args) (parse-apply-arguments args)])
        (b:apply b:apply fn args))))

; implementation of apply when keyword arguments are supplied
(define (apply/kws kws kw-vals . args)
  (when (b:empty? args)
    (b:apply raise-arity-error 'apply (arity-at-least 1) args))
  (let-values ([(fn args) (parse-apply-arguments args)])
    (b:apply b:keyword-apply fn kws kw-vals args)))

; just like b:apply, but converts the last argument to a list from an arbitrary sequence
(define apply (make-keyword-procedure apply/kws apply/basic))

; lazily concatenates sequences
(define (append . seqs)
  (if (b:empty? seqs) b:empty-stream
      (foldl concatenated-sequence (b:first seqs) (b:rest seqs))))

; conj over multiple items
(define (conj* seq . items)
  (foldl conj seq items))

; extend over multiple sequences
(define (extend* seq . seqs)
  (foldl extend seq seqs))

; lazy filter
(define (filter pred seq)
  (if (empty? seq) b:empty-stream
      (let ([head (first seq)]
            [tail (rest seq)])
        (if (pred head)
            (b:stream-cons head (filter pred tail))
            (filter pred tail)))))

; lazy map
(define (map proc . seqs)
  (let loop ([seqs* seqs])
    (cond
    [(andmap empty? seqs*)
     b:empty-stream]
    [(ormap empty? seqs*)
     (raise-arguments-error
      'map "all sequences must have the same length"
      "proc" proc
      "sequences" seqs)]
    [else
     (b:stream-cons (b:apply proc (b:map first seqs*))
                    (loop (b:map rest seqs*)))])))

; strict fold
(define (foldl proc init . seqs)
  (let loop ([init* init]
             [seqs* seqs])
    (cond
    [(andmap empty? seqs*)
     init*]
    [(ormap empty? seqs*)
     (raise-arguments-error
      'foldl "all sequences must have the same length"
      "proc" proc
      "init" init
      "sequences" seqs)]
    [else
     (loop (b:apply proc init* (b:map first seqs*)) (b:map rest seqs*))])))

; nth abbreviations
(define (second seq)  (nth seq 1))
(define (third seq)   (nth seq 2))
(define (fourth seq)  (nth seq 3))
(define (fifth seq)   (nth seq 4))
(define (sixth seq)   (nth seq 5))
(define (seventh seq) (nth seq 6))
(define (eighth seq)  (nth seq 7))
(define (ninth seq)   (nth seq 8))
(define (tenth seq)   (nth seq 9))

(define (sequence->list seq)
  (reverse (extend '() seq)))

; using ‘in’ outside of a for clause converts a sequence to a stream
(define/contract in/proc
  (sequence? . -> . b:stream?)
  (let () ; this is done to get the compiler to statically infer the name as ‘in’, not ‘in-proc’
    (define (in seq)
      (if (empty? seq) b:empty-stream
          (b:stream-cons (first seq) (in/proc (rest seq)))))
    in))

; if used in a for clause, it uses the sequence directly
(define-sequence-syntax in
  (λ () #'in/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(e) (_ seq)]
       #'[(e)
          (:do-in
           ([(s) seq])
           (unless (sequence? s)
             (raise-argument-error 'in "sequence?" s))
           ([v s])
           (not (empty? v))
           ([(e) (first v)]
            [(r) (rest v)])
           #t #t
           [r])]])))

(define-syntaxes (for/sequence for*/sequence)
  (let ()
    (define ((make-for/sequence name derived-stx) stx)
      (syntax-case stx ()
        [(_ clauses . body)
         (begin
           (when (null? (syntax->list #'body))
             (raise-syntax-error name
                                 "missing body expression after sequence bindings"
                                 stx #'body))
           #`(sequence->stream
              (in-generator
               (#,derived-stx
                #,stx () clauses
                (yield (let () . body))
                (values)))))]))
    (values (make-for/sequence 'for/sequence #'for/fold/derived)
            (make-for/sequence 'for*/sequence #'for*/fold/derived))))
