#lang racket/base

(require racket/generic
         (prefix-in b: racket/base)
         racket/contract
         racket/set
         racket/dict
         racket/stream)

(provide
 gen:countable countable? countable/c
 (contract-out
  [length (countable? . -> . exact-nonnegative-integer?)]
  [known-finite? (countable? . -> . boolean?)]))

(define-generics countable
  (length countable)
  (known-finite? countable)
  #:fallbacks
  [(define (known-finite? c) #f)]
  #:fast-defaults
  ([list? (define length b:length)
          (define (known-finite? c) #t)])
  #:defaults
  ([vector? (define length vector-length)
            (define (known-finite? c) #t)]
   [string? (define length string-length)
            (define (known-finite? c) #t)]
   [bytes? (define length bytes-length)
           (define (known-finite? c) #t)]
   [hash? (define length hash-count)
          (define (known-finite? c) #t)]
   [set? (define length set-count)
         (define (known-finite? c) #t)]
   [dict? (define length dict-count)]
   [stream? (define length stream-length)]))
