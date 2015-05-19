#lang racket/base

(require racket/generic
         racket/lazy-require
         "collection.rkt")

(provide
 gen:indexable indexable? indexable/c
 ref)

(define-generics indexable
  (ref indexable . _)
  #:defaults
  ([hash? (define ref hash-ref)]
   [sequence? (define ref nth)]))
