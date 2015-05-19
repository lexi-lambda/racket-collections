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
  [length (countable? . -> . exact-nonnegative-integer?)]))

(define-generics countable
  (length countable)
  #:fast-defaults
  ([list? (define length b:length)])
  #:defaults
  ([vector? (define length vector-length)]
   [string? (define length string-length)]
   [bytes? (define length bytes-length)]
   [hash? (define length hash-count)]
   [set? (define length set-count)]
   [dict? (define length dict-count)]
   [stream? (define length stream-length)]))
