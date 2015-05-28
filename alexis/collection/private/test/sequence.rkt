#lang racket/base

(require
  rackunit
  alexis/collection
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
 (check-equal? (nth '(a b c) 1) 'b)
 (check-equal? (nth (stream 1 2 3) 1) 2))

(test-case
 "Functional sequence updates"
 (check-equal? (set-nth '(a b) 0 'c) '(c b))
 (check-equal? (update-nth '(1 2) 0 add1) '(2 2))
 (check-equal? (sequence->list (set-nth (stream 'a 'b) 1 'c)) '(a c))
 (check-equal? (sequence->list (update-nth (stream 1 2) 1 add1)) '(1 3))
 (check-equal? (sequence->list (set-nth (rest #(a b c)) 1 'd)) '(b d))
 (check-equal? (update-nth #(1 2 3) 1 add1) #(1 3 3)))

(test-case
 "Sequence reversal"
 (check-equal? (reverse '(1 2 3)) '(3 2 1))
 (check-equal? (extend '() (reverse #(1 2 3))) '(1 2 3))
 (check-equal? (sequence->list (reverse (stream 1 2 3 4))) '(4 3 2 1)))

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
 (check-equal? (length (append '(1 2) '(3 4))) 4)
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

(test-case
 "Sequence for clause iteration"
 (check-pred procedure? in)
 (check-pred stream? (in #(1 2 3)))
 (check-equal? (sequence->list (in #(1 2 3))) '(1 2 3))
 (check-equal? (for/list ([x (in #(1 2 3))]) (add1 x)) '(2 3 4))
 (check-exn exn:fail:contract? (thunk (for ([i (in 'not-a-sequence)]) (void)))))

(test-case
 "Derived for/sequence loops"
 (check-equal? (extend #() (for/sequence ([i (in-range 10)]) (* i i)))
               #(0 1 4 9 16 25 36 49 64 81))
 (check-equal? (extend #() (for*/sequence ([x (in-range 1 5)]
                                           [y (in-range 1 5)]) (* x y)))
               #(1 2 3 4 2 4 6 8 3 6 9 12 4 8 12 16)))

(test-case
 "Special sequence errors on mutable builtins"
 (check-exn #rx"which is mutable" (thunk (empty? (vector))))
 (check-exn #rx"which is mutable" (thunk (empty? (make-hash))))
 (check-exn #rx"which is mutable" (thunk (empty? (mutable-set))))
 (check-exn #rx"expected: sequence\\?\n" (thunk (empty? 'not-a-sequence))))

(test-case
 "Good error messages for finite sequences"
 (check-exn #rx"index is out of range" (thunk (nth #(1 2 3 4) 4)))
 (check-exn #rx"index is out of range" (thunk (set-nth #(1 2 3 4) 4 'x)))
 (check-exn #rx"index is out of range" (thunk (update-nth #(1 2 3 4) 4 void))))
