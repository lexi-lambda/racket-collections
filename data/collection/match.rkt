#lang racket/base

(require
  (for-syntax racket/base
              racket/list
              syntax/parse
              syntax/parse/experimental/template)
  data/collection/countable
  data/collection/collection
  data/collection/sequence
  racket/match)

(provide sequence)

(define ((length-at-least n) seq)
  (if (and (countable? seq)
           (known-finite? seq))
      (>= (length seq) n)
      (let loop ([n n]
                 [seq seq])
        (cond [(zero? n) #t]
              [(empty? seq) #f]
              [else (loop (sub1 n) (rest seq))]))))

(define-match-expander sequence
  (λ (stx)
    (define-splicing-syntax-class svp
      #:attributes [pat multi?]
      (pattern (~seq pat:expr (~literal ...))
               #:attr multi? #t)
      (pattern pat:expr
               #:attr multi? #f))
    (syntax-parse stx
      ; if the arg list is empty, just expand to predicates
      [(_) #'(and (? sequence?) (? empty?))]
      ; otherwise do actual pattern matching
      [(_ svp:svp ...)
       (define multi? (attribute svp.multi?))
       (define last-multi? (last multi?))
       (define other-multi? (ormap values (drop-right multi? 1)))
       (cond
         ; if there are any internal greedy multi-matches, force the whole sequence to a list
         [other-multi?
          (template (? sequence? (app sequence->list (list (?@ . svp) ...))))]
         ; if the last one is a multi-match, match it as a lazy sequence
         [last-multi?
          (define init-pats (drop-right (syntax->list #'(svp.pat ...)) 1))
          (with-syntax ([(init-pat ...) init-pats]
                        [last-pat (last (syntax->list #'(svp.pat ...)))]
                        [init-len (length init-pats)])
            #'(and (? sequence?) (? (length-at-least init-len))
                   (app (λ (seq) (sequence->list (take init-len seq))) (list init-pat ...))
                   (and c (app (λ (seq) (drop init-len seq)) last-pat))))]
         ; otherwise just match the first elements as pats
         [else
          (define pats (syntax->list #'(svp.pat ...)))
          (with-syntax ([(pat ...) pats]
                        [len (length pats)])
            #'(? sequence?
                 (and (app (λ (seq) (sequence->list (take len seq))) (list pat ...))
                      (app (λ (seq) (empty? (drop len seq))) #t))))])])))
