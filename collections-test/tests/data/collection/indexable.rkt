#lang racket/base

(require rackunit
         data/collection
         racket/set
         racket/stream
         racket/generic
         racket/function) 

(struct my-list (items)
  #:transparent
  #:methods gen:indexable
  [(define/generic -ref ref)
   (define/generic -set-ref set-ref)
   (define (ref indexable . args)
     (define pos (car args))
     (list-ref (my-list-items indexable)
               pos))
   (define (set-ref indexable . args)
     (define pos (car args))
     (define item (cadr args))
     (define ls (my-list-items indexable))
     (my-list (-set-ref ls pos item)))])

(test-case "ref"
  "list"
  (check-equal? (ref '(a b) 1) 'b)
  "vector"
  (check-equal? (ref #(a b c) 1) 'b)
  "hash"
  (check-equal? (ref (hash 'a 'b) 'a) 'b)
  "set"
  (check-equal? (ref (set 'a 'b) 1) 'b)
  "alist"
  (check-equal? (ref '((a . b)) 'a) 'b)
  "custom data type my-list"
  (check-equal? (ref (my-list '(a b)) 1) 'b))

(test-case "set-ref"
  "list"
  (check-equal? (set-ref '(a b) 0 'c)
                '(c b))
  "hash"
  (check-equal? (set-ref (hash 'a 'b) 'a 'c)
                (hash 'a 'c))
  "set"
  (check-equal? (stream->list (set-ref (set 0 1) 0 3))
                (stream->list (stream 3 0)))
  "alist"
  (check-equal? (set-ref '((a . b)) 'a 'c)
                '((a . c)))
  "custom data type my-list"
  (check-equal? (set-ref (my-list '(a b)) 0 'c)
                (my-list '(c b))))

(test-case "update-ref"
  "list"
  (check-equal? (update-ref '(a b) 0 identity)
                '(a b))
  "hash"
  (check-equal? (update-ref (hash 'a 'b) 'a identity)
                (hash 'a (identity 'b)))
  "set"
  (check-equal? (stream->list (update-ref (set 'a 'b) 0 identity))
                (stream->list (stream 'a 'b)))
  "alist"
  (check-equal? (update-ref '((a . b)) 'a identity)
                `((a . ,(identity 'b))))
  (check-equal? (update-ref (my-list '(a b)) 1 identity)
                (my-list `(a ,(identity 'b)))))
