#lang racket/base

(require
  scribble/manual
  scribble/eval)

(provide
 reftech
 coll-evaluator
 coll-interaction
 coll-examples)

(define (reftech . content)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") content))

(define coll-evaluator
  (make-eval-factory
   #:lang 'racket
   '(racket/generic
     data/collection
     data/collection/experimental/quasi
     racket/generator)))

(define-syntax-rule (coll-interaction . body)
  (interaction #:eval (coll-evaluator) . body))

(define-syntax-rule (coll-examples . body)
  (examples #:eval (coll-evaluator) . body))
