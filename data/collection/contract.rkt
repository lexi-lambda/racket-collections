#lang racket/base

(require
  data/collection/collection
  racket/contract
  racket/generic
  racket/function
  unstable/function
  racket/stream
  racket/set)

(provide
 (contract-out
  [sequenceof ([contract?] [#:chaperone? any/c] . ->* . contract?)]))

(define (add-sequence-context blame)
  (blame-add-context blame "a value in"))

; a helper function for impersonating or chaperoning sequences
(define (redirect-sequence seq chaperone? empty?* first* rest* nth* reverse* . props)
  (define redir-proc (if chaperone? chaperone-procedure impersonate-procedure))
  (redirect-generics
   chaperone? gen:sequence seq
   [empty? (λ (empty?) (and empty? (redir-proc empty? empty?*)))]
   [first (λ (first) (and first (redir-proc first first*)))]
   [rest (λ (rest) (and rest (redir-proc rest rest*)))]
   [nth (λ (nth) (and nth (redir-proc nth nth*)))]
   [reverse (λ (reverse) (and reverse (redir-proc reverse reverse*)))]
   #:properties props))

;; ---------------------------------------------------------------------------------------------------

(define (sequenceof-name ctc)
  (build-compound-type-name 'sequenceof (base-sequenceof-content ctc)))

(define (check-sequenceof! ctc val blame)
  (unless (sequence? val)
    (raise-blame-error blame val '(expected: "a sequence" given: "~e") val)))

(define ((sequenceof-first-order ctc) val)
  (sequence? val))

(define (sequenceof-stronger? a b)
  (contract-stronger? (base-sequenceof-content a)
                      (base-sequenceof-content b)))

(define (raise-unsupported-type-error blame val type)
  (raise-blame-error
   blame val
   '(expected: "a chaperoneable sequence" given: "~e, and ~a is not chaperoneable") val type))

(define ((ho-projection chaperone?) ctc)
  (let ([elem-ctc (base-sequenceof-content ctc)])
    (λ (blame)
      (let* ([passthrough-blame (blame-add-context blame #f)]
             [sequence-blame (add-sequence-context blame)]
             [pos-elem-proj ((contract-projection elem-ctc) sequence-blame)]
             [neg-elem-proj ((contract-projection elem-ctc) (blame-swap sequence-blame))])
        (define (attach val)
          (check-sequenceof! ctc val sequence-blame)
          (cond
            [(empty? val) val]
            ; check for lists to get nicer contract errors
            [(list? val)
             (((contract-projection (listof elem-ctc)) passthrough-blame) val)]
            [(pair? val)
             (((contract-projection (cons/c elem-ctc ctc)) passthrough-blame) val)]
            [((conjoin vector? immutable?) val)
             (((contract-projection (vectorof elem-ctc #:immutable #t)) sequence-blame) val)]
            [(set? val)
             (((contract-projection (set/c elem-ctc #:kind 'immutable)) passthrough-blame) val)]
            [(stream? val)
             (((contract-projection (stream/c elem-ctc)) passthrough-blame) val)]
            ; force some values to streams for the contract checking
            ; (only allowed for non-chaperone contracts)
            [((conjoin hash? immutable?) val)
             (when chaperone? (raise-unsupported-type-error sequence-blame val 'hash?))
             (((contract-projection (stream/c elem-ctc)) passthrough-blame)
              (sequence->stream (in-hash-pairs val)))]
            [((conjoin string? immutable?) val)
             (when chaperone? (raise-unsupported-type-error sequence-blame val 'string?))
             (((contract-projection (stream/c elem-ctc)) passthrough-blame)
              (sequence->stream (in-string val)))]
            [((conjoin bytes? immutable?) val)
             (when chaperone? (raise-unsupported-type-error sequence-blame val 'bytes?))
             (((contract-projection (stream/c elem-ctc)) passthrough-blame)
              (sequence->stream (in-bytes val)))]
            [else
             (redirect-sequence
              val chaperone?
              values
              (λ (seq) (values (λ (result) (pos-elem-proj result)) seq))
              (λ (seq) (values (λ (result) (attach result)) seq))
              (λ (seq n) (values (λ (result) (pos-elem-proj result)) seq n))
              (λ (seq) (values (λ (result) (attach result)) seq))
              impersonator-prop:contracted ctc
              impersonator-prop:blame sequence-blame)]))
        attach))))

(struct base-sequenceof (content))

(struct chaperone-sequenceof base-sequenceof ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name sequenceof-name
   #:first-order sequenceof-first-order
   #:stronger sequenceof-stronger?
   #:projection (ho-projection #t)))

(struct impersonator-sequenceof base-sequenceof ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name sequenceof-name
   #:first-order sequenceof-first-order
   #:stronger sequenceof-stronger?
   #:projection (ho-projection #f)))

(define (sequenceof v #:chaperone? [chaperone? #f])
  (define ctc (coerce-contract 'sequenceof v))
  (if chaperone?
      (chaperone-sequenceof ctc)
      (impersonator-sequenceof ctc)))
