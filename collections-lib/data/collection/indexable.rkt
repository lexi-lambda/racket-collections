#lang racket/base

(require racket/generic
         racket/function
         racket/dict
         "collection.rkt")

(provide
 gen:indexable indexable? indexable/c
 ref set-ref update-ref)

(define-generics indexable
  (ref indexable . _)
  (set-ref indexable . _)
  (update-ref indexable . _)
  #:defaults
  ([hash? (define ref hash-ref)
          (define set-ref hash-set)
          (define update-ref hash-update)]
   [dict? (define ref dict-ref)
          (define set-ref dict-set)
          (define update-ref dict-update)]
   [sequence? (define ref nth)
              (define set-ref set-nth)
              (define update-ref update-nth)]
   )
  #:fallbacks
  [(define/generic ref* ref)
   (define/generic set-ref* set-ref)
   (define (update-ref indexable index proc)
     (let ([old (ref* indexable index)])
       (set-ref* indexable index (proc old))))])
