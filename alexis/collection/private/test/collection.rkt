#lang racket/base

(require
  rackunit
  alexis/collection
  racket/set
  racket/function)

(test-case
 "Basic collection operations (conj)"
 (check-equal? (conj '() 'a) '(a))
 (check-equal? (conj '(a) 'b) '(b a))
 (check-equal? (conj #() 'a) #(a))
 (check-equal? (conj #(a) 'b) #(a b))
 (check-equal? (conj (hash) '(a . b)) (hash 'a 'b))
 (check-equal? (conj (hash 'a 'b) '(c . d)) (hash 'a 'b 'c 'd))
 (check-equal? (conj (hash 'a 'b) '(a . c)) (hash 'a 'c))
 (check-equal? (conj (set) 'a) (set 'a))
 (check-equal? (conj (set 'a) 'b) (set 'a 'b))
 (check-equal? (conj (set 'a) 'a) (set 'a)))

(test-case
 "Basic collection operations (extend)"
 (check-equal? (extend '() '(a b c)) '(c b a))
 (check-equal? (extend '() #(a b c)) '(c b a))
 (check-equal? (extend '() (hash 'a 'b)) '((a . b)))
 (check-equal? (extend #() '(a b c)) #(a b c))
 (check-equal? (extend #() #(a b c)) #(a b c))
 (check-equal? (extend (hash) '((a . b) (c . d))) (hash 'a 'b 'c 'd))
 (check-exn exn:fail:contract? (thunk (extend (hash) '(a b c)))))

(test-case
 "Collection abbreviations"
 (check-equal? (conj* '() 'a 'b 'c) '(c b a))
 (check-equal? (extend* '() '(a b c) #(1 2 3) (hash 'foo 'bar)) '((foo . bar) 3 2 1 c b a)))
