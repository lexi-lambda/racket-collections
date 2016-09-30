#lang racket/base

(require
  rackunit
  data/collection
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
 "Lazy fold steps"
 (check-equal? (sequence->list (foldl/steps conj #() '(1 2 3))) '(#() #(1) #(1 2) #(1 2 3)))
 (check-equal? (sequence->list (foldl/steps list '() '(1 2) '(3 4))) '(() (() 1 3) ((() 1 3) 2 4)))
 (check-equal? (with-output-to-string
                (thunk (foldl/steps (λ (_ n) (display n)) (void) '(1 2 3))))
               "")
 (check-equal? (with-output-to-string
                (thunk (first (foldl/steps (λ (_ n) (display n)) (void) '(1 2 3)))))
               "")
 (check-equal? (with-output-to-string
                (thunk (second (foldl/steps (λ (_ n) (display n)) (void) '(1 2 3)))))
               "1")
 (check-equal? (with-output-to-string
                (thunk (sequence->list (foldl/steps (λ (_ n) (display n)) (void) '(1 2 3)))))
               "123"))

(test-case
 "Logical fold abbreviations"
 (check-equal? (andmap symbol? '(a 1 c d)) #f)
 (check-equal? (andmap symbol? '(a b c d)) #t)
 (check-equal? (andmap values '(a b c d)) 'd)
 (check-equal? (ormap symbol? '(1 2 3 4)) #f)
 (check-equal? (ormap symbol? '(1 a 3 4)) #t)
 (check-equal? (ormap values '(#f a #f #f)) 'a))

(test-case
 "find-best, find-min, and find-max"
 (check-equal? (find-best '("pears" "bananas" "apples") string<?) "apples")
 (check-equal? (find-best '("pears" "bananas" "apples") string>?) "pears")

 (check-equal? (find-best '((3 pears) (1 banana) (2 apples)) string<?
                          #:key (compose1 symbol->string second))
               '(2 apples))
 (check-equal? (find-best '((3 pears) (1 banana) (2 apples)) string>?
                          #:key (compose1 symbol->string second))
               '(3 pears))

 (check-equal? (find-min '(8 5 0 -2 5)) -2)
 (check-equal? (find-max '(8 5 0 -2 5)) 8)

 (check-equal? (find-min '((3 pears) (1 banana) (2 apples)) #:key first) '(1 banana))
 (check-equal? (find-min '((1 banana) (1 orange))           #:key first) '(1 banana))
 (check-equal? (find-max '((3 pears) (1 banana) (2 apples)) #:key first) '(3 pears))
 (check-equal? (find-max '((3 pears) (3 oranges))           #:key first) '(3 pears)))

(test-case
 "Extra sequence operations"
 (check-equal? (last '(1 2 3 4)) 4)
 (check-equal? (last #(1 2 3 4)) 4)
 (check-equal? (last (stream 1 2 3 4)) 4)
 (check-equal? (last (take 5 (in-naturals))) 4)
 (check-equal? (first (drop 2 (in-naturals))) 2)
 (check-equal? (reverse (extend '() (subsequence (in-naturals) 1 5))) '(1 2 3 4)))

(test-case
 "Finding element indicies"
 (check-equal? (index-of '(1 2 3) 2) 1)
 (check-equal? (index-of '(1 2 3) 4) #f)
 (check-equal? (index-of '(1 2 3) 2 (const #f)) #f)

 (check-equal? (index-where '(1 2 3) positive?) 0)
 (check-equal? (index-where '(-1 2 3) positive?) 1)
 (check-equal? (index-where '(-1 -2 -3) positive?) #f))

(test-case
 "Indexed sequence construction"
 (check-equal? (sequence->list (take 5 (build-sequence values))) '(0 1 2 3 4))
 (check-equal? (sequence->list (take 5 (build-sequence add1))) '(1 2 3 4 5))
 (check-equal? (sequence->list (take 5 (build-sequence (λ _ 'a)))) '(a a a a a))

 (check-equal? (sequence->list (build-sequence 5 values)) '(0 1 2 3 4))
 (check-equal? (sequence->list (build-sequence 5 add1)) '(1 2 3 4 5))
 (check-equal? (sequence->list (build-sequence 5 (λ _ 'a))) '(a a a a a)))

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

(test-case
 "Flattening operations"
 (check-true (empty? (flatten '((((())))))))
 (check-equal? (sequence->list (flatten '((1 2) 3 (((4)))))) '(1 2 3 4))
 (check-equal? (nth (flatten (repeat (repeat '(1)))) 1000) 1)
 (check-equal? (second (append-map values (repeat (repeat 1)))) 1))

(test-case
 "Indexed sequences"
 (check-equal? (sequence->list (indexed '(a b c))) '((0 . a) (1 . b) (2 . c)))
 (check-equal? (extend (hash) (indexed '(a b c))) #hash((0 . a) (1 . b) (2 . c))))

(test-case
 "Sequence chunking"
 (check-equal? (sequence->list* (chunk 2 (range 10))) '((0 1) (2 3) (4 5) (6 7) (8 9)))
 (check-equal? (sequence->list* (chunk 3 (range 10))) '((0 1 2) (3 4 5) (6 7 8) (9)))
 (check-exn exn:fail:contract? (thunk (sequence->list* (chunk* 3 (range 10))))))

(test-case
 "cartesian-product"
 (check-equal? (sequence->list* (cartesian-product))
               '(()))
 (check-equal? (sequence->list* (cartesian-product (range 20) '() (naturals)))
               '())
 (check-equal? (sequence->list* (cartesian-product '(1 2) '(a b) '(c d)))
               '((1 a c) (1 a d) (1 b c) (1 b d) (2 a c) (2 a d) (2 b c) (2 b d)))
 (check-equal? (sequence->list* (cartesian-product '(1 2 3) '(a b c)))
               '((1 a) (1 b) (1 c) (2 a) (2 b) (2 c) (3 a) (3 b) (3 c)))
 (check-equal? (sequence->list* (cartesian-product '(4 5 6) '(d e f) '(#t #f)))
               '((4 d #t)
                 (4 d #f)
                 (4 e #t)
                 (4 e #f)
                 (4 f #t)
                 (4 f #f)
                 (5 d #t)
                 (5 d #f)
                 (5 e #t)
                 (5 e #f)
                 (5 f #t)
                 (5 f #f)
                 (6 d #t)
                 (6 d #f)
                 (6 e #t)
                 (6 e #f)
                 (6 f #t)
                 (6 f #f)))
 (check-equal? (sequence->list* (take 10 (cartesian-product (naturals) '(a b) '(c d))))
               '((0 a c) (0 a d) (0 b c) (0 b d) (1 a c) (1 a d) (1 b c) (1 b d) (2 a c) (2 a d))))

(test-case
 "Built-in infinite sequences"
 (check-equal?
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed 0)
    (sequence->list (take 10 (randoms 10 (current-pseudo-random-generator)))))
  '(8 6 2 4 8 4 5 3 2 6)))

(test-case
 "Total element removal"
 (check-equal? (sequence->list (remove-all '(a b c d a b d e) 'a)) '(b c d b d e))
 (check-equal? (sequence->list (remove-all (stream 'a 'b 'c 'd 'a 'b 'd 'e) 'a)) '(b c d b d e))
 (check-equal? (sequence->list (remove-all #(a b c d a b d e) 'a)) '(b c d b d e)))

(test-case
 "Single element removal"
 (check-equal? (sequence->list (remove-first '(a b c d a b d e) 'a)) '(b c d a b d e))
 (check-equal? (sequence->list (remove-first '(a b c d a b d e) 'f)) '(a b c d a b d e))
 (check-equal? (remove-first '(a b c d a b d e) 'f equal? (thunk #f)) #f))
