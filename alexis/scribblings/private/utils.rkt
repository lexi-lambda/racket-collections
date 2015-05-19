#lang racket/base

(require scribble/manual)

(provide reftech)

(define (reftech . content)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") content))