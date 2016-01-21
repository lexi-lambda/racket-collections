#lang racket/base

(require racket/contract
         racket/list)

(provide
 (contract-out
  [tuple-listof ([] #:rest (listof contract?) . ->* . contract?)]
  [list-ending-with (contract? . -> . contract?)]))

; adapted from plistof from unstable/gui/redex
(define (tuple-listof . ctcs)
  (define ctc
    (recursive-contract
     (or/c null? tuple/c)))
  (define tuple/c
    (foldr cons/c ctc ctcs))
  ctc)

; a contract for lists that terminate with a particular element; used by `apply`
(define (add-list-ending-with-context blame)
  (blame-add-context blame "the last element of"))

(define (list-ending-with-name ctc)
  (build-compound-type-name 'list-ending-with (base-list-ending-with-ctc ctc)))

(define ((list-ending-with-first-order ctc) val)
  (and (list? val)
       (not (empty? val))
       (contract-first-order-passes? (base-list-ending-with-ctc ctc) (last val))))

(define (list-ending-with-stronger? a b)
  (contract-stronger? (base-list-ending-with-ctc a)
                      (base-list-ending-with-ctc b)))

(define ((list-ending-with-ho-projection flat?) ctc)
  (let* ([elem-ctc (base-list-ending-with-ctc ctc)]
         [pos-elem-first-proj (contract-projection elem-ctc)])
    (λ (blame)
      (let* ([passthrough-blame (blame-add-context blame #f)]
             [context-blame (add-list-ending-with-context blame)]
             [pos-elem-proj (pos-elem-first-proj context-blame)])
        (λ (val)
          (when (empty? val)
            (raise-blame-error passthrough-blame val
                               '(given: "'()" expected: "a non-empty list")))
          (if flat?
              (pos-elem-proj (last val))
              (foldr (λ (elem acc) (cons (if (empty? acc)
                                             (pos-elem-proj elem)
                                             elem)
                                         acc))
                     '() val)))))))

(struct base-list-ending-with (ctc))

(struct flat-list-ending-with base-list-ending-with ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name list-ending-with-name
   #:first-order list-ending-with-first-order
   #:stronger list-ending-with-stronger?
   #:projection (list-ending-with-ho-projection #t)
   #:list-contract? (λ _ #t)))

(struct chaperone-list-ending-with base-list-ending-with ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name list-ending-with-name
   #:first-order list-ending-with-first-order
   #:stronger list-ending-with-stronger?
   #:projection (list-ending-with-ho-projection #f)))

(struct impersonator-list-ending-with base-list-ending-with ()
  #:property prop:contract
  (build-contract-property
   #:name list-ending-with-name
   #:first-order list-ending-with-first-order
   #:stronger list-ending-with-stronger?
   #:projection (list-ending-with-ho-projection #f)))

(define (list-ending-with v)
  (let ([ctc (coerce-contract 'list-ending-with v)])
    (cond [(flat-contract? ctc)      (flat-list-ending-with ctc)]
          [(chaperone-contract? ctc) (chaperone-list-ending-with ctc)]
          [else                      (impersonator-list-ending-with ctc)])))
