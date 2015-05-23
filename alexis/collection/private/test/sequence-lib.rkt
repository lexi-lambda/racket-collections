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
 "Extra sequence operations"
 (check-equal? (last '(1 2 3 4)) 4)
 (check-equal? (last #(1 2 3 4)) 4)
 (check-equal? (last (stream 1 2 3 4)) 4)
 (check-equal? (last (take 5 (in-naturals))) 4)
 (check-equal? (first (drop 2 (in-naturals))) 2)
 (check-equal? (reverse (extend '() (subsequence (in-naturals) 1 5))) '(1 2 3 4)))
