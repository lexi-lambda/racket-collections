#lang racket/base

(require
  rackunit
  alexis/collection/collection
  racket/set
  racket/stream
  racket/function
  (only-in racket/list range)
  racket/string)

(test-case
 "Basic sequence operations (empty?)"
 (check-true (empty? '()))
 (check-false (empty? '(a)))
 (check-true (empty? #()))
 (check-false (empty? #(a)))
 (check-true (empty? (hash)))
 (check-false (empty? (hash 'a 'b)))
 (check-true (empty? (set)))
 (check-false (empty? (set 'a)))
 (check-true (empty? empty-stream))
 (check-false (empty? (stream 'a))))

(test-case
 "Basic sequence operations (first / rest / nth)"
 (check-equal? (first '(a b)) 'a)
 (check-equal? (rest '(a b)) '(b))
 (check-equal? (first #(a b)) 'a)
 (check-equal? (first (rest #(a b))) 'b)
 (check-equal? (nth '(a b c) 1) 'b))

(test-case
 "Sequence reversal"
 (check-equal? (reverse '(1 2 3)) '(3 2 1))
 (check-equal? (extend '() (reverse #(1 2 3))) '(1 2 3)))

(test-case
 "Sequence-based application"
 (check-equal? (apply +) 0)
 (check-equal? (apply + #(1 1 1)) 3)
 (check-equal? (apply + 1 1 #(1)) 3)
 (check-equal? (apply string-replace #("foo" "o" "a")) "faa")
 (check-equal? (apply string-replace #:all? #f #("foo" "o" "a")) "fao")
 (check-exn exn:fail:contract? (thunk (apply 'not-a-fn #())))
 (check-exn exn:fail:contract? (thunk (apply + 'not-a-seq)))
 (check-exn exn:fail:contract? (thunk (apply)))
 (check-exn exn:fail:contract? (thunk (apply #:kw 'any))))

(test-case
 "Sequence concatenation"
 (check-true (empty? (append)))
 (check-equal? (extend #() (append '(1 2) '(3 4))) #(1 2 3 4))
 (check-equal? (extend #() (append #(1 2) (hash 3 4))) #(1 2 (3 . 4))))

(test-case
 "Sequence filtering"
 (check-equal? (extend #() (filter positive? '(-2 -1 0 1 2))) #(1 2)))

(test-case
 "Sequence mapping"
 (check-equal? (extend #() (map add1 '(1 2 3))) #(2 3 4))
 (check-exn exn:fail:contract? (thunk (extend null (map + '(1 2) '(3))))))

(test-case
 "Sequence folding"
 (check-equal? (foldl + 0 (set 1 2 3)) 6)
 (check-exn exn:fail:contract? (thunk (foldl + 0 '(1 2) '(3)))))

(test-case
 "Sequence indexing abbreviations"
 (check-equal? (second (range 10)) 1)
 (check-equal? (third (range 10)) 2)
 (check-equal? (fourth (range 10)) 3)
 (check-equal? (fifth (range 10)) 4)
 (check-equal? (sixth (range 10)) 5)
 (check-equal? (seventh (range 10)) 6)
 (check-equal? (eighth (range 10)) 7)
 (check-equal? (ninth (range 10)) 8)
 (check-equal? (tenth (range 10)) 9))
