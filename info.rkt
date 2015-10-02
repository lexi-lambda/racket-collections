#lang info

(define collection 'multi)

(define name "alexis-collections")
(define version "0.2.0")

(define deps
  '("alexis-util"
    "base"
    "rackunit-lib"))
(define build-deps
  '("cover"
    "cover-coveralls"
    "racket-doc"
    "scribble-lib"))
