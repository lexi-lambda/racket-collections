#lang info

(define collection 'multi)

(define name "collections")
(define version "1.0")

(define deps
  '(["base" "6.2"]
    "match-plus"
    "rackunit-lib"
    "static-rename"
    "unstable-list-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
