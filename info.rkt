#lang info

(define collection 'multi)

(define name "collections")
(define version "1.0")

(define deps
  '("alexis-util"
    "base"
    "rackunit-lib"
    "unstable-list-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
