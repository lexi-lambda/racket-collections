#lang info

(define collection 'multi)

(define name "collections")
(define version "1.0")

(define deps
  '("alexis-util"
    ["base" "6.2"]
    "match-plus"
    "rackunit-lib"
    "unstable-list-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
