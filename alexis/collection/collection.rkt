#lang racket/base

(require racket/generic
         racket/contract
         unstable/function
         alexis/util/match
         alexis/util/renamed
         (prefix-in b: racket/base)
         (prefix-in b: racket/list)
         (prefix-in b: racket/vector)
         (prefix-in b: racket/set)
         (prefix-in b: racket/stream))

(provide
 gen:collection collection? collection/c
 gen:sequence sequence? sequence/c
 first rest apply
 (contract-out
  ; gen:collection
  [extend (sequence? sequence? . -> . sequence?)]
  [conj (sequence? any/c . -> . sequence?)]
  ; gen:sequence
  [empty? (sequence? . -> . boolean?)]
  [nth (sequence? exact-nonnegative-integer? . -> . any)]
  ; derived functions
  [extend* ([sequence?] #:rest (listof sequence?) . ->* . sequence?)]
  [conj* ([sequence?] #:rest any/c . ->* . sequence?)]
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

;; fallbacks
;; ---------------------------------------------------------------------------------------------------

(define (-conj coll item)
  (extend coll (list item)))

(define (-extend coll coll*)
  (foldl conj coll coll*))

(define (-first seq)
  (nth seq 0))

(define (-nth seq index)
  (if (zero? index)
      (first seq)
      (nth (rest seq) (sub1 index))))

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
    (define conj b:set-add)]))

; a sequence is an ordered set of values
(define-generics sequence
  (empty? sequence)
  (first sequence)
  (rest sequence)
  (nth sequence index)
  #:fallbacks
  [(define first -first)
   (define nth -nth)]
  #:fast-defaults
  ([pair?
    (define empty? b:null?)
    (define first b:car)
    (define rest b:cdr)])
  #:defaults
  ([(conjoin vector? immutable?)
    (define empty? (compose1 zero? vector-length))
    (define nth vector-ref)
    (define rest (compose1 b:stream-rest b:sequence->stream b:in-vector))]
   [b:stream?
    (define empty? b:stream-empty?)
    (define first b:stream-first)
    (define rest b:stream-rest)]
   [(conjoin hash? immutable?)
    (define empty? hash-empty?)
    (define (hash->stream ht) (b:sequence->stream (b:in-hash-pairs ht)))
    (define first (compose1 b:stream-first hash->stream))
    (define rest (compose1 b:stream-rest hash->stream))]
   [b:set?
    (define empty? b:set-empty?)
    (define first (compose1 b:stream-first b:set->stream))
    (define rest (compose1 b:stream-rest b:set->stream))]))

;; derived functions
;; ---------------------------------------------------------------------------------------------------

; two sequences concatenated together in order, for laziness
(struct concatenated-sequence (a b)
  #:reflection-name 'lazy-sequence
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
      (b:first args)
      (let-values ([(fn args) (parse-apply-arguments args)])
        (b:apply b:apply fn args))))

; implementation of apply when keyword arguments are supplied
(define (apply/kws kws kw-vals . args)
  (when (b:empty? args)
    (b:apply raise-arity-error 'apply (arity-at-least 1) args))
  (if (= (b:length args) 1)
      (b:first args)
      (let-values ([(fn args) (parse-apply-arguments args)])
        (b:apply b:keyword-apply fn kws kw-vals args))))

; just like b:apply, but converts the last argument to a list from an arbitrary sequence
(define apply (make-keyword-procedure apply/kws apply/basic))

; lazily concatenates sequences
(define (append . seqs)
  (if (b:empty? seqs) b:empty-stream
      (foldl concatenated-sequence (b:first seqs) (b:rest seqs))))

; conj over multiple items
(define (conj* seq . items)
  (b:foldl conj seq items))

; extend over multiple sequences
(define (extend* seq . seqs)
  (b:foldl extend seq seqs))

; lazy filter
(define (filter pred seq)
  (let ([head (first seq)]
        [tail (rest seq)])
    (if head
        (b:stream-cons head (filter pred tail))
        (filter pred tail))))

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
