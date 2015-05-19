#lang info

(define collection 'multi)

(define name "alexis-collections")
(define version "0.2.0")

(define deps
  '("base"
    "unstable-list-lib"
    "alexis-util"))
(define build-deps
  '("scribble-lib"
    "racket-doc"
    "cover"))