#lang racket/base

(require data/collection/experimental/quasi
         racket/stream
         rackunit)

(test-case
 "Simple successful quasiquotation"
 (check-equal? `(1 . #(,(+ 1 1)
                       #hash((a . #s(sct ,(string-append "a" "b" "c")))
                             (b . ,(- 4 3)))
                       4))
               '(1 . #(2
                       #hash((a . #s(sct "abc"))
                             (b . 1))
                       4))))

(test-case
 "Splicing quasiquotation with sequences"
 (check-equal? `(1 ,@(list 2 3 4)
                   ,@(stream 5 6 7)
                   #hash((a . #s(sct i ,@(vector-immutable 'ii 'iii 'iv) v))))
               '(1 2 3 4 5 6 7
                   #hash((a . #s(sct i ii iii iv v))))))
