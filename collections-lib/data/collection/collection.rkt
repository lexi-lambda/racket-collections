#lang racket/base

;; This contains the base implementations for gen:collection and gen:sequence, as well as some derived
;; functions to operate on them.

(require racket/require
         (for-syntax racket/base
                     syntax/parse)
         (multi-in racket [contract function generator generic lazy-require match])
         (prefix-in b: (multi-in racket [base dict list set stream vector]))
         (prefix-in u: (multi-in unstable [function list]))
         (submod racket/performance-hint begin-encourage-inline)
         match-plus
         static-rename
         "countable.rkt"
         "private/util.rkt")

; lazily depend on random-access.rkt since it depends on this
(lazy-require
 ["sequence.rkt" [sequence->string]]
 ["private/random-access.rkt" [wrap-random-access-sequence]])

(provide
 gen:collection (rename-out [collection?* collection?]) collection/c
 gen:sequence (rename-out [sequence?* sequence?]) sequence/c
 in for/sequence for*/sequence for/sequence/derived for*/sequence/derived
 (contract-out
  ; gen:collection
  [extend (collection?* sequence?* . -> . collection?*)]
  [conj (collection?* any/c . -> . collection?*)]
  ; gen:sequence
  [empty? (sequence?* . -> . boolean?)]
  [first ((and/c sequence?* (not/c empty?)) . -> . any)]
  [rest ((and/c sequence?* (not/c empty?)) . -> . any)]
  [rename nth* nth (sequence?* exact-nonnegative-integer? . -> . any)]
  [rename set-nth* set-nth (sequence?* exact-nonnegative-integer? any/c . -> . sequence?*)]
  [rename update-nth* update-nth (sequence?* exact-nonnegative-integer? (any/c . -> . any/c)
                                             . -> . sequence?*)]
  [reverse (sequence?* . -> . sequence?*)]
  [sequence->collection (sequence?* . -> . collection?*)]
  [random-access? (sequence?* . -> . boolean?)]
  ; derived functions
  [extend* ([collection?*] #:rest (listof sequence?*) . ->* . sequence?*)]
  [conj* ([collection?*] #:rest any/c . ->* . sequence?*)]
  [rename set-nth** set-nth* ([sequence?*]
                              #:rest (tuple-listof exact-nonnegative-integer? any/c)
                              . ->* . sequence?*)]
  [rename update-nth** update-nth* ([sequence?*]
                                    #:rest (tuple-listof exact-nonnegative-integer?
                                                         (any/c . -> . any/c))
                                    . ->* . sequence?*)]
  [apply apply/c]
  [append ([] #:rest (listof sequence?*) . ->* . sequence?*)]
  [filter ((any/c . -> . any/c) sequence?* . -> . sequence?*)]
  [map (->i ([proc (seqs) (and/c (procedure-arity-includes/c (b:length seqs))
                                 (unconstrained-domain-> any/c))])
            #:rest [seqs (non-empty-listof sequence?*)]
            [result sequence?*])]
  [foldl (->i ([proc (seqs) (and/c (procedure-arity-includes/c (add1 (b:length seqs)))
                                   (unconstrained-domain-> any/c))]
               [init any/c])
              #:rest [seqs (non-empty-listof sequence?*)]
              [result any/c])]
  [sequence->list (sequence?* . -> . list?)]
  [sequence->list* (sequence?* . -> . list?)]
  ; helpers
  [second (sequence?* . -> . any)]
  [third (sequence?* . -> . any)]
  [fourth (sequence?* . -> . any)]
  [fifth (sequence?* . -> . any)]
  [sixth (sequence?* . -> . any)]
  [seventh (sequence?* . -> . any)]
  [eighth (sequence?* . -> . any)]
  [ninth (sequence?* . -> . any)]
  [tenth (sequence?* . -> . any)]))

;; wrappers
;; ---------------------------------------------------------------------------------------------------

; provide nice range errors for countable sequences
(define (nth* seq i)
  (when (and (countable? seq)
             (known-finite? seq)
             (>= i (length seq)))
    (raise-range-error 'nth "sequence" "" i seq 0 (sub1 (length seq))))
  (nth seq i))

(define (set-nth* seq i v)
  (when (and (countable? seq)
             (known-finite? seq)
             (>= i (length seq)))
    (raise-range-error 'set-nth "sequence" "" i seq 0 (sub1 (length seq))))
  (set-nth seq i v))

(define (update-nth* seq i p)
  (when (and (countable? seq)
             (known-finite? seq)
             (>= i (length seq)))
    (raise-range-error 'update-nth "sequence" "" i seq 0 (sub1 (length seq))))
  (update-nth seq i p))

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

(define (-rest seq)
  (if (and (random-access? seq) (sequence-implements? seq 'nth))
      (rest (wrap-random-access-sequence seq))
      (raise-support-error 'rest seq)))

(define (-reverse seq)
  (if (and (random-access? seq) (sequence-implements? seq 'nth))
      (reverse (wrap-random-access-sequence seq))
      (raise-support-error 'reverse seq)))

(define (-nth seq index)
  (if (zero? index)
      (first seq)
      (nth (rest seq) (sub1 index))))

(define (-set-nth seq index value)
  (let loop ([seq seq]
             [index index])
    (if (zero? index)
        (b:stream-cons value (in (rest seq)))
        (b:stream-cons (first seq) (loop (rest seq) (sub1 index))))))

(define (-update-nth seq index proc)
  (cond
    [(and (random-access? seq) (sequence-implements? seq 'nth 'set-nth))
     (set-nth seq index (proc (nth seq index)))]
    [else
     (let loop ([seq seq]
                [index index])
       (if (zero? index)
           (b:stream-cons (proc (first seq)) (in (rest seq)))
           (b:stream-cons (first seq) (loop (rest seq) (sub1 index)))))]))

(define (-sequence->collection seq)
  (if (collection? seq) seq
      (appending-collection seq)))

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
   [(u:conjoin vector? immutable?)
    (define (conj vec item)
      (b:vector->immutable-vector
       (b:vector-append vec (b:vector item))))]
   [(u:conjoin hash? immutable?)
    (define/contract (conj hsh item)
      ((and/c hash? immutable?) pair? . -> . (and/c hash? immutable?))
      (hash-set hsh (car item) (cdr item)))]
   [b:set?
    (define conj b:set-add)]
   [(u:conjoin b:dict? (negate b:dict-mutable?) b:dict-can-functional-set?)
    (define/contract (conj dct item)
      ((and/c b:dict? (negate b:dict-mutable?) b:dict-can-functional-set?)
       pair? . -> . (and/c b:dict? (negate b:dict-mutable?) b:dict-can-functional-set?))
      (b:dict-set dct (car item) (cdr item)))]
   [b:stream?
    (define (conj strm item) (b:stream-cons item strm))]))

; a sequence is an ordered set of values
(define-generics sequence
  (empty? sequence)
  (first sequence)
  (rest sequence)
  (nth sequence index)
  (set-nth sequence index value)
  (update-nth sequence index proc)
  (reverse sequence)
  (sequence->collection sequence)
  (random-access? sequence)
  #:defined-predicate sequence-implements?
  #:fallbacks
  [(define empty? -empty?)
   (define first -first)
   (define rest -rest)
   (define nth -nth)
   (define set-nth -set-nth)
   (define update-nth -update-nth)
   (define reverse -reverse)
   (define sequence->collection -sequence->collection)
   (define (random-access? seq) #f)]
  #:derive-property prop:sequence (λ (s) (in s))
  #:defaults
  ([list?
    (define empty? b:null?)
    (define first b:car)
    (define rest b:cdr)
    (define reverse b:reverse)
    (define nth list-ref)
    (define set-nth u:list-set)
    (define update-nth u:list-update)]
   [(u:conjoin vector? immutable?)
    (define nth vector-ref)
    (define (set-nth vec i v)
      (let ([copy (b:vector-copy vec)])
        (vector-set! copy i v)
        (vector->immutable-vector copy)))
    (define (random-access? v) #t)]
   [b:stream?
    (define empty? b:stream-empty?)
    (define first b:stream-first)
    (define rest b:stream-rest)
    (define reverse stream-reverse)]
   [(u:conjoin hash? immutable?)
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
   [(u:conjoin b:dict? (negate b:dict-mutable?))
    (define empty? b:dict-empty?)
    (define first (compose1 b:stream-first b:sequence->stream b:in-dict-pairs))
    (define rest (compose1 b:stream-rest b:sequence->stream b:in-dict-pairs))
    (define reverse (compose1 stream-reverse b:sequence->stream b:in-dict-pairs))]
   [(u:conjoin string? immutable?)
    (define nth string-ref)
    (define rest (compose1 b:stream-rest b:sequence->stream in-string))
    (define reverse (compose1 sequence->string stream-reverse b:sequence->stream in-string))
    (define (random-access? s) #t)]
   [(u:conjoin bytes? immutable?)
    (define nth bytes-ref)
    (define rest (compose1 b:stream-rest b:sequence->stream in-bytes))
    (define reverse (compose1 stream-reverse b:sequence->stream in-bytes))
    (define (random-access? b) #t)]))

; create custom flat contracts to provide nice error messages for mutable builtins
(define sequence?*
  (make-flat-contract
   #:name 'sequence?
   #:first-order sequence?
   #:projection
   (λ (blame)
     (λ (val)
       (cond
         [(sequence? val) val]
         [((u:disjoin vector? hash? b:set-mutable? b:set-weak? b:dict? string? bytes?) val)
          (raise-blame-error
           blame val
           '(expected: "sequence?, which must be immutable" given: "~e, which is mutable") val)]
         [else
          (raise-blame-error blame val '(expected: "sequence?" given: "~e") val)])))))

(define collection?*
  (make-flat-contract
   #:name 'collection?
   #:first-order collection?
   #:projection
   (λ (blame)
     (λ (val)
       (cond
         [(collection? val) val]
         [((u:disjoin vector? hash? b:set-mutable? b:set-weak? b:dict?) val)
          (raise-blame-error
           blame val
           '(expected: "collection?, which must be immutable" given: "~e, which is mutable") val)]
         [else
          (raise-blame-error blame val '(expected: "collection?" given: "~e") val)])))))

;; utility implementations
;; ---------------------------------------------------------------------------------------------------

(struct appending-collection (seq)
  #:methods gen:collection
  [(define/match* (conj (appending-collection seq) e)
     (appending-collection (append seq (list e))))]
  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -set-nth set-nth)
   (define/match* (empty? (appending-collection seq)) (-empty? seq))
   (define/match* (first (appending-collection seq)) (-first seq))
   (define/match* (rest (appending-collection seq)) (-rest seq))
   (define/match* (set-nth (appending-collection seq) i v) (-set-nth seq i v))])

;; derived functions
;; ---------------------------------------------------------------------------------------------------

; helper for make-apply-wrapper
(begin-encourage-inline
  (define (contract-positional-args args blame)
    ; fail fast if the arity is wrong
    (when (< (b:length args) 2)
      (apply raise-arity-error 'apply (arity-at-least 2) args))

    ; ensure the first argument is a procedure
    (define initial (b:first args))
    (define initial-blame (blame-add-context blame "the first argument of"))
    (define initial-contracted (((contract-projection procedure?) initial-blame) initial))
    
    ; ensure the last argument is a sequence
    (define-values [middle final] (b:split-at (b:rest args) (- (b:length args) 2)))
    (define final-blame (blame-add-context blame "the last argument of"))
    (define final-contracted (((contract-projection sequence?*) final-blame) (car final)))

    ; return all the arguments with contracts properly applied
    (b:cons initial-contracted (b:append middle (list final-contracted)))))

; creates a wrapper for chaperoning apply
(define (make-apply-wrapper blame)
  (make-keyword-procedure
   (λ (kws kw-vals . args) (b:apply values kw-vals (contract-positional-args args blame)))
   (λ args                 (b:apply values         (contract-positional-args args blame)))))

; the actual implementation of apply/c
(define (apply/c-projection blame)
  (let ([wrapper-fn (make-apply-wrapper (blame-swap blame))])
    (λ (val) (chaperone-procedure val wrapper-fn))))

; a custom contract for apply to enable using the special sequence?* error message
(define apply/c
  (make-chaperone-contract
   #:name '(-> procedure? any/c ... sequence? any)
   #:first-order procedure?
   #:projection apply/c-projection))

; validates the arguments passed to apply and converts the final argument to a list from a sequence
(define (parse-apply-arguments args)
  (define fn (b:first args))
  (define main-args (b:rest args))
  (define-values (init last) (b:split-at main-args (sub1 (length main-args))))
  (define last* (sequence->list (b:first last)))
  (values fn (b:append init (list last*))))

; implementation of apply when no keyword arguments are supplied
(define/renamed apply (apply/basic . args)
  (let-values ([(fn args) (parse-apply-arguments args)])
    (b:apply b:apply fn args)))

; implementation of apply when keyword arguments are supplied
(define (apply/kws kws kw-vals . args)
  (let-values ([(fn args) (parse-apply-arguments args)])
    (b:apply b:keyword-apply fn kws kw-vals args)))

; just like b:apply, but converts the last argument to a list from an arbitrary sequence
(define apply (make-keyword-procedure apply/kws apply/basic))

; lazily concatenates sequences
(define (append . seqs)
  (for*/sequence ([seq (in-list seqs)]
                  [e (in seq)])
    e))

; conj over multiple items
(define (conj* seq . items)
  (foldl conj seq items))

; extend over multiple sequences
(define (extend* seq . seqs)
  (foldl extend seq seqs))

; set-nth over multiple values
(define/match (set-nth** seq . args)
  [(_ (list)) seq]
  [(_ (list n v args ...))
   (apply set-nth** (set-nth* seq n v) args)])

; update-nth over multiple values
(define/match (update-nth** seq . args)
  [(_ (list)) seq]
  [(_ (list n proc args ...))
   (apply update-nth** (update-nth* seq n proc) args)])

; lazy filter
(define (filter pred seq)
  (if (empty? seq) b:empty-stream
      (let ([head (first seq)]
            [tail (rest seq)])
        (if (pred head)
            (b:stream-cons head (filter pred tail))
            (filter pred tail)))))

; lazy map
(define map
  (case-lambda
    [(proc seq)
     (let loop ([seq* seq])
       (cond
         [(empty? seq*)
          b:empty-stream]
         [else
          (b:stream-cons (proc (first seq*))
                         (loop (rest seq*)))]))]
    [(proc . seqs)
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
                         (loop (b:map rest seqs*)))]))]))

; strict fold
(define foldl
  (case-lambda
    [(proc init seq)
     (let loop ([init* init]
                [seq* seq])
       (cond
         [(empty? seq*)
          init*]
         [else
          (loop (proc init* (first seq*)) (rest seq*))]))]
    [(proc init . seqs)
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
          (loop (b:apply proc init* (b:map first seqs*)) (b:map rest seqs*))]))]))

; nth abbreviations
(define (second seq)  (nth* seq 1))
(define (third seq)   (nth* seq 2))
(define (fourth seq)  (nth* seq 3))
(define (fifth seq)   (nth* seq 4))
(define (sixth seq)   (nth* seq 5))
(define (seventh seq) (nth* seq 6))
(define (eighth seq)  (nth* seq 7))
(define (ninth seq)   (nth* seq 8))
(define (tenth seq)   (nth* seq 9))

; simple abbreviation to avoid manually reversing the result list
(define (sequence->list seq)
  (reverse (extend '() seq)))

; like sequence->list, but deep
(define (sequence->list* seq)
  (for/list ([e (in seq)])
    (if (sequence? e)
        (sequence->list* e)
        e)))

; using ‘in’ outside of a for clause converts a sequence to a stream
(define/contract in/proc
  (sequence?* . -> . b:stream?)
  (let () ; this is done to get the compiler to statically infer the name as ‘in’, not ‘in/proc’
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

(define-syntaxes (for/sequence/derived for*/sequence/derived)
  (let ()
    (define ((make-for/sequence/derived derived-stx) stx)
      (syntax-parse stx
        [(_ name:id clauses . body)
         (begin
           (when (null? (syntax->list #'body))
             (raise-syntax-error (syntax-e #'name)
                                 "missing body expression after sequence bindings"
                                 stx #'body))
           #`(sequence->stream
              (in-generator
               (#,derived-stx
                (name clauses . body) () clauses
                (yield (let () . body))
                (values)))))]))
    (values (make-for/sequence/derived #'for/fold/derived)
            (make-for/sequence/derived #'for*/fold/derived))))

(define-syntax-rule (for/sequence . rest)
  (for/sequence/derived for/sequence . rest))
(define-syntax-rule (for*/sequence . rest)
  (for*/sequence/derived for*/sequence . rest))
