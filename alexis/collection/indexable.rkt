#lang racket/base

(require racket/generic
         racket/function
         unstable/function
         racket/dict
         "collection.rkt")

(provide
 gen:indexable indexable? indexable/c
 ref set-ref)

(define-generics indexable
  (ref indexable . _)
  (set-ref indexable . _)
  #:defaults
  ([hash? (define ref hash-ref)
          (define set-ref hash-set)]
   [dict? (define ref dict-ref)
          (define set-ref dict-set)]
   [sequence? (define ref nth)
              (define set-ref set-nth)]))
