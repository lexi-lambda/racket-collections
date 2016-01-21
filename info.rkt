#lang info

(define collection 'multi)

(define name "collections")
(define version "1.1")

(define deps
  '(["base" "6.2"]
    "match-plus"
    "rackunit-lib"
    "static-rename"
    "kw-utils"
    "unstable-list-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
