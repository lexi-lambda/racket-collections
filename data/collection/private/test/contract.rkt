#lang racket/base

(require
  rackunit
  data/collection
  data/collection/contract
  racket/contract
  racket/function
  racket/stream
  racket/set)

(test-case
 "Contracts on defaults"
 (check-not-exn
  (thunk (contract (sequenceof integer?) null
                   'pos 'neg)))
 (check-exn
  #rx"broke its own contract"
  (thunk (contract (sequenceof integer?)
                   '(1 2 a 3 4 5)
                   'pos 'neg)))
 (check-exn
  #rx"broke its own contract"
  (thunk (nth (contract (sequenceof integer?)
                        `(1 2 3 . ,(stream 'a))
                        'pos 'neg)
              3)))
 (check-exn
  #rx"broke its own contract"
  (thunk (contract (sequenceof integer?)
                   #(1 2 a 3 4 5)
                   'pos 'neg)))
 (check-exn
  #rx"hash\\? is not chaperoneable"
  (thunk (contract (sequenceof integer? #:chaperone? #t)
                   (hash 1 2 3 4)
                   'pos 'neg)))
 (check-exn
  #rx"string\\? is not chaperoneable"
  (thunk (contract (sequenceof char? #:chaperone? #t)
                   "abcd"
                   'pos 'neg)))
 (check-exn
  #rx"bytes\\? is not chaperoneable"
  (thunk (contract (sequenceof byte? #:chaperone? #t)
                   #"abcd"
                   'pos 'neg)))
 (check-exn
  #rx"broke its own contract"
  (thunk (first (contract (sequenceof integer?)
                          (hash 1 2 3 4)
                          'pos 'neg))))
 (check-exn
  #rx"broke its own contract"
  (thunk (contract (sequenceof integer?)
                   (set 1 2 'a 3 4 5)
                   'pos 'neg))))

(test-case
 "Contracts on custom sequences"
 (struct my-seq ()
   #:methods gen:sequence
   [(define (first seq) 'datum)
    (define (rest seq) (my-seq))
    (define (empty? seq) #f)])
 (check-not-exn
  (thunk (first (contract (sequenceof symbol?)
                          (my-seq)
                          'pos 'neg))))
 (check-exn
  #rx"broke its own contract"
  (thunk (first (contract (sequenceof integer?)
                          (my-seq)
                          'pos 'neg)))))
