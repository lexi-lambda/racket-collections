#lang scribble/manual

@(require
   racket/require
   (for-label (subtract-in
               (combine-in
                racket/base
                racket/list
                racket/set
                racket/dict
                racket/math)
               data/collection)
              (prefix-in base: racket/base)
              data/collection
              racket/function
              racket/generic
              racket/contract
              racket/stream
              racket/string
              racket/match
              racket/generator)
   scribble/core
   "../private/utils.rkt")

@(module racket-forms racket/base
   (require (for-label racket/base)
            scribble/manual)
   (provide racket:quasiquote)
   (define racket:quasiquote
     (racket quasiquote)))
@(require 'racket-forms)

@title[#:tag "experimental"]{Experimental Forms and Functions}

@(define (yellow . content)
   (make-element (make-style #f (list (make-background-color-property "yellow"))) content))

@nested[#:style 'inset]{
 @yellow{@bold{WARNING}}: The following forms and functions are @emph{experimental}; compatibility
         will not be maintained.}

@section{Generic Quasiquotation}

@defmodule[data/collection/experimental/quasi]

@defform[(quasiquote datum)]{
Equivalent to @racket:quasiquote from @racketmodname[racket/base], except that uses of
@racket[unquote-splicing] accept arbitrary @racket[sequence?] values instead of only lists.

Note that this handling only applies to uses of @racket[unquote-splicing], not @racket[unquote], so
@racket[`(1 @#,racketvalfont{.} ,_more)] will produce different results from @racket[`(1 ,@_more)]
when @racket[_more] is a non-list sequence.

@(coll-examples
  `(1 2 ,(+ 1 2))
  `(Z = ,@(take 5 (naturals)) ...)
  `#s(prefab i ,@#(ii iii iv) v))}
