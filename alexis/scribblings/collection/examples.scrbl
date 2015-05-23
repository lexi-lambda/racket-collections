#lang scribble/manual

@(require
   racket/require
   (for-label (subtract-in
               (combine-in
                racket/base
                racket/list
                racket/set
                racket/dict)
               alexis/collection)
              (prefix-in base: racket/base)
              alexis/collection
              racket/generic
              racket/contract
              racket/stream
              racket/string
              racket/match)
   scribble/eval
   "../private/utils.rkt")

@title[#:tag "collections-examples"]{Examples}

Obviously, the primary purpose of generic collections is the ability to operate on values in a
collection-agnostic way. For example, it is possible to map over all kinds of sequences, even
sequences of heterogenous types.

@(coll-interaction
  (sequence->list (map + #(1 2 3) (stream 10 20 30))))

However, more interesting applications of generic collections involve the ability to use lazy
sequences to create infinite streams of values. For example, it is possible to create an infinite
stream of all the Fibonacci numbers:

@(coll-interaction
  (define fibs (stream-cons 1 (stream-cons 1 (map + fibs (rest fibs)))))
  (sequence->list (take 15 fibs)))

Similarly, here is an implementation of the classic
@hyperlink["http://en.wikipedia.org/wiki/Fizz_buzz#Other_uses"]{“fizz buzz” problem} that uses
infinite sequences:

@(coll-interaction
  (define ((divisible-by? x) n)
    (zero? (remainder n x)))
  (define fizzbuzz
    (map
     (match-lambda
       [(? (divisible-by? 15)) 'fizzbuzz]
       [(? (divisible-by?  3)) 'fizz]
       [(? (divisible-by?  5)) 'buzz]
       [n                      n])
     (in-naturals 1)))
  (sequence->list (take 20 fizzbuzz)))
