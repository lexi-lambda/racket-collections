#lang racket/base

(require
  rackunit
  alexis/collection
  racket/function
  racket/stream
  racket/port)

(test-case
 "Side-effectful for-each"
 (check-equal?
  (with-output-to-string
   (thunk (for-each display (take 5 (in-naturals)))))
  "01234"))

(test-case
 "Logical fold abbreviations"
 (check-equal? (andmap symbol? '(a 1 c d)) #f)
 (check-equal? (andmap symbol? '(a b c d)) #t)
 (check-equal? (andmap values '(a b c d)) 'd)
 (check-equal? (ormap symbol? '(1 2 3 4)) #f)
 (check-equal? (ormap symbol? '(1 a 3 4)) #t)
 (check-equal? (ormap values '(#f a #f #f)) 'a))

(test-case
 "Extra sequence operations"
 (check-equal? (last '(1 2 3 4)) 4)
 (check-equal? (last #(1 2 3 4)) 4)
 (check-equal? (last (stream 1 2 3 4)) 4)
 (check-equal? (last (take 5 (in-naturals))) 4)
 (check-equal? (first (drop 2 (in-naturals))) 2)
 (check-equal? (reverse (extend '() (subsequence (in-naturals) 1 5))) '(1 2 3 4)))

(test-case
 "Infinite sequence constructors"
 (check-equal? (first (repeat 'foo)) 'foo)
 (check-equal? (nth (repeat 'foo) 10000) 'foo)
 (check-exn exn:fail:contract? (thunk (cycle '())))
 (check-equal? (sequence->list (take 6 (cycle '(1 2 3)))) '(1 2 3 1 2 3)))

(test-case
 "Sequence to string and bytestring conversions"
 (check-equal? (sequence->string #(#\a #\b #\c)) "abc")
 (check-equal? (sequence->bytes #(1 2 3)) #"\1\2\3"))
