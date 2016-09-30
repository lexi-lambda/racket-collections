#lang info

(define collection 'multi)

(define name "collections")
(define version "1.3.1")

(define deps
  '(["base" #:version "6.2"]
    "curly-fn-lib"
    ["functional-lib" #:version "0.3.1"]
    "match-plus"
    "rackunit-lib"
    "static-rename"
    "unstable-list-lib"))
(define build-deps
  '("functional-doc"
    "racket-doc"
    "scribble-lib"))
