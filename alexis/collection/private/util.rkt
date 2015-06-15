#lang racket/base

(require
  racket/contract)

(provide
 (contract-out
  [tuple-listof ([] #:rest (listof contract?) . ->* . contract?)]))

; adapted from plistof from unstable/gui/redex
(define (tuple-listof . ctcs)
  (define ctc
    (recursive-contract
     (or/c null? tuple/c)))
  (define tuple/c
    (foldr cons/c ctc ctcs))
  ctc)
