#lang racket/base

(require
  rackunit
  alexis/collection
  alexis/util/match
  racket/generic
  racket/dict)

(struct my-dict (assocs)
  #:transparent
  #:methods gen:dict
  [(define/generic -dict-ref dict-ref)
   (define/generic -dict-set dict-set)
   (define/generic -dict-remove dict-remove)
   (define/generic -dict-count dict-count)
   (define/generic -dict-iterate-first dict-iterate-first)
   (define/generic -dict-iterate-next dict-iterate-next)
   (define/generic -dict-iterate-key dict-iterate-key)
   (define/generic -dict-iterate-value dict-iterate-value)
   (define (dict-ref dct k [fail #f]) (-dict-ref (my-dict-assocs dct) k))
   (define (dict-set dct k v) (my-dict (-dict-set (my-dict-assocs dct) k v)))
   (define (dict-remove dct k) (my-dict (-dict-remove (my-dict-assocs dct) k)))
   (define (dict-count dct) (-dict-count (my-dict-assocs dct)))
   (define (dict-iterate-first dct) (-dict-iterate-first (my-dict-assocs dct)))
   (define (dict-iterate-next dct pos) (-dict-iterate-next (my-dict-assocs dct) pos))
   (define (dict-iterate-key dct pos) (-dict-iterate-key (my-dict-assocs dct) pos))
   (define (dict-iterate-value dct pos) (-dict-iterate-value (my-dict-assocs dct) pos))])

(define my-dict-null (my-dict null))

(test-case
 "Dicts as collections"
 (check-equal? (conj my-dict-null '(a . b)) (my-dict '((a . b))))
 (check-equal? (conj (my-dict '((a . b))) '(c . d)) (my-dict '((a . b) (c . d))))
 (check-equal? (conj (my-dict '((a . b))) '(a . d)) (my-dict '((a . d))))
 (check-equal? (extend my-dict-null '((a . b) (c . d))) (my-dict '((a . b) (c . d)))))

(test-case
 "Dicts as sequences"
 (check-true (empty? my-dict-null))
 (check-false (empty? (my-dict '((a . b)))))
 (check-equal? (ref (my-dict '((a . b))) 'a) 'b)
 (check-equal? (nth (my-dict '((a . b))) 0) '(a . b)))
