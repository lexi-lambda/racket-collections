#lang racket/base

(require (for-syntax racket/base)
         data/collection
         syntax/parse/define
         (only-in racket/base [cons pair]))

(provide quasiquote unquote unquote-splicing)

(begin-for-syntax
  (define-syntax-class qqed
    #:attributes [expr]
    #:description #f
    #:literals [unquote]
    [pattern (uq:unquote ~! e:expr)
             #:attr expr (syntax-property #'e
                                          'disappeared-use
                                          (syntax-local-introduce #'uq))]
    [pattern x
             #:attr expr #'(quasiquote x)])

  (define-syntax-class qqed/splicing
    #:attributes [expr]
    #:description #f
    #:literals [unquote-splicing]
    [pattern (uqs:unquote-splicing ~! e)
             #:declare e (expr/c #'sequence?
                                 #:macro 'unquote-splicing
                                 #:name "argument")
             #:attr expr (syntax-property #'e.c
                                          'disappeared-use
                                          (syntax-local-introduce #'uqs))]
    [pattern e:qqed
             #:attr expr #`(list #,(attribute e.expr))]))

(define-syntax-parser quasiquote
  [(_ (x:qqed/splicing ...))
   #'(sequence->list (append x.expr ...))]
  [(_ #(x:qqed/splicing ...))
   #'(extend #() (append x.expr ...))]
  [(_ (a:qqed . b:qqed))
   #'(pair a.expr b.expr)]
  [(_ prefab)
   #:when (prefab-struct-key (syntax-e #'prefab))
   #:with key (prefab-struct-key (syntax-e #'prefab))
   #:with #(_ x:qqed/splicing ...) (struct->vector (syntax-e #'prefab))
   #'(apply make-prefab-struct 'key (append x.expr ...))]
  [(_ hsh)
   #:do [(define datum (syntax-e #'hsh))]
   #:when (hash? datum)
   #:with [(k . v:qqed) ...] (hash->list datum)
   #:with ctor (cond [(hash-eq? datum) #'make-immutable-hasheq]
                     [(hash-eqv? datum) #'make-immutable-hasheqv]
                     [else #'make-immutable-hash])
   #'(ctor (list (pair 'k v.expr) ...))]
  [(_ other)
   #''other])
