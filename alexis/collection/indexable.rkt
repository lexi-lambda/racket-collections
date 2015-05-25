#lang racket/base

(require racket/generic
         racket/function
         unstable/function
         racket/dict
         "collection.rkt")

(provide
 gen:indexable indexable? indexable/c
 ref)

(define-generics indexable
  (ref indexable . _)
  #:defaults
  ([hash? (define ref hash-ref)]
   [dict? (define ref dict-ref)]
   [sequence? (define ref nth)]))
