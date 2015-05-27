#lang racket/base

(require alexis/collection/collection
         alexis/collection/countable
         alexis/util/match
         racket/generic
         racket/contract)

(provide
 (contract-out
  [wrap-random-access-sequence (sequence? . -> . sequence?)]))

(define (wrap-random-access-sequence seq)
  (random-access-sequence seq 0))

(struct random-access-sequence (sequence offset)
  ; the length is just the internal length minus the offset
  #:methods gen:countable
  [(define/generic -length length)
   (define/match* (length (random-access-sequence seq i))
     (- (-length seq) i))]
  ; we get first and empty? for free
  #:methods gen:sequence
  [(define/generic -nth nth)
   ; just increment the offset for rest
   (define/match* (rest (random-access-sequence seq i))
     (random-access-sequence seq (add1 i)))
   ; offset the lookup
   (define/match* (nth (random-access-sequence seq i) i*)
     (-nth seq (+ i i*)))
   ; just wrap the whole thing in a layer of indirection for reversal
   (define (reverse ra-seq)
     (reversed-sequence ra-seq))])

(struct reversed-sequence (seq)
  #:reflection-name 'random-access-sequence
  ; the length is just the internal length
  #:methods gen:countable
  [(define/generic -length length)
   (define/match* (length (reversed-sequence seq))
     (-length seq))]
  ; we get first and empty? for free
  #:methods gen:sequence
  [(define/generic -nth nth)
   ; just wrap the whole thing in a random access iterator for iteration
   (define (rest rev-seq)
     (random-access-sequence rev-seq 1))
   ; reverse the lookup
   (define/match* (nth (reversed-sequence seq) i)
     (-nth seq (- (length seq) i 1)))
   ; if the sequence is reversed, we can just throw away the shell
   (define/match* (reverse (reversed-sequence seq))
     seq)])
