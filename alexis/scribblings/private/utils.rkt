#lang racket/base

(require
  scribble/manual
  scribble/eval)

(provide
 reftech
 coll-evaluator
 coll-examples)

(define (reftech . content)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") content))

(define coll-evaluator
  (make-eval-factory
   #:lang 'racket
   '(racket/generic
     alexis/collection)))

(define-syntax-rule (coll-examples . body)
  (examples #:eval (coll-evaluator) . body))
