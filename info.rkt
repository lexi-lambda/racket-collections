#lang info

(define collection 'multi)

(define name "collections")
(define version "1.2")

(define deps
  '(["base" "6.2"]
    "curly-fn"
    "match-plus"
    "rackunit-lib"
    "static-rename"
    "unstable-list-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
