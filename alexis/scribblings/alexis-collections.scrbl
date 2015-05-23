#lang scribble/manual

@(require
   racket/require
   (for-label (subtract-in
               (combine-in
                racket/base
                racket/list
                racket/set
                racket/dict)
               alexis/collection)
              (prefix-in base: racket/base)
              alexis/collection
              racket/generic
              racket/contract
              racket/stream)
   scribble/eval
   "private/utils.rkt")

@(define evaluator
   (make-eval-factory
    #:lang 'racket
    '(racket/generic
      alexis/collection)))

@title{Generic Collections}

@defmodule[alexis/collection]

This provides a set of @reftech{generic interfaces} for built-in Racket collections to create a
unified interface for working with Racket data structures.
@seclink["structures" #:doc '(lib "scribblings/reference/reference.scrbl")]{User-defined structures}
may also implement the collections API to provide implementations for additional datatypes.

This collection provides a number of bindings that override bindings in @racketmodname[racket/base],
some with slightly different semantics in addition to support for multiple kinds of collections. For
this reason, this @emph{may} not be a drop-in replacement for existing code.

@section{Generic Collections and Sequences}

@tech{Generic sequences} are the bulk of this library, providing a uniform interface for interacting
with collections. Sequences are distinct from Racket @reftech{sequences}, which are a different, much
more ad-hoc concept.

A @deftech{generic collection} is any structure that can contain values, while a @deftech{generic
sequence} represents a sequence of ordered values.

@subsection{Collections}

@defthing[gen:collection any/c]{

A @reftech{generic interface} that represents any structure that can contain values. The
@racket[gen:collection] interface only provides two functions, @racket[conj] and @racket[extend].

The following built-in datatypes have implementations for @racket[gen:collection]:

@itemlist[
 @item{@reftech{lists}}
 @item{@emph{immutable} @reftech{hash tables}}
 @item{@emph{immutable} @reftech{vectors}}
 @item{@emph{immutable} @reftech{hash sets}}]

@(examples
  #:eval (evaluator)
  (conj #(1 2 3) 4)
  (extend '() (hash 'a "b" 'c "d")))}

@defproc[(collection? [v any/c]) boolean?]{
A predicate that determines if @racket[v] is a @tech{generic collection}.}

@subsubsection[#:tag "collection-methods"]{Generic Methods}

@defproc[(conj [coll collection?] [item any/c]) collection?]{
Returns a new collection with all the items in @racket[coll], plus @racket[item]. There is no
requirement for @emph{where} the value is added to the collection—@reftech{lists} prepend elements
@italic{a la} @racket[cons], while @reftech{vectors} append them.

If @racket[extend] is implemented but not @racket[conj], an implementation will automatically be
provided.}

@defproc[(extend [a collection?] [b collection?]) collection?]{
Returns a new collection with all the items in both @racket[a] and @racket[b], and the result is the
same kind of collection as @racket[a].

If @racket[conj] is implemented but not @racket[extend], an implementation will automatically be
provided.}

@subsubsection[#:tag "collection-functions"]{Derived Functions}

@defproc[(conj* [coll collection?] [item any/c] ...) collection?]{
Repeatedly calls @racket[conj] for each @racket[item] provided, in order.}

@defproc[(extend* [base collection?] [extension collection?] ...) collection?]{
Repeatedly calls @racket[extend] for each @racket[extension] provided, in order.}

@subsection{Sequences}

@defthing[gen:sequence any/c]{

A @reftech{generic interface} that represents any ordered sequence of values. The
@racket[gen:sequence] interface provides the @racket[empty?], @racket[first], @racket[rest], and
@racket[nth] functions.

The following built-in datatypes have implementations for @racket[gen:sequence]:

@itemlist[
 @item{@reftech{lists}}
 @item{@emph{immutable} @reftech{hash tables}}
 @item{@emph{immutable} @reftech{vectors}}
 @item{@emph{immutable} @reftech{hash sets}}
 @item{@reftech{streams}}]

@(examples
  #:eval (evaluator)
  (extend (set) (map + '(1 2 3) #(4 5 6))))}

@defproc[(sequence? [v any/c]) boolean?]{
A predicate that determines if @racket[v] is a @tech{generic sequence}.}

@subsubsection[#:tag "sequence-methods"]{Generic Methods}

@defproc[(empty? [seq sequence?]) boolean?]{
Determines if @racket[seq] has no values.

All implementations of @racket[gen:sequence] are required to implement this method, unless they also
implement @racket[gen:countable].}

@defproc[(first [seq (and/c sequence? (not/c empty?))]) any/c]{
Retrieves the first values in @racket[seq].

This method is optional if an implementation of @racket[nth] is provided.}

@defproc[(rest [seq (and/c sequence? (not/c empty?))]) any/c]{
Retrieves a new sequence which represents all but the first value in @racket[seq].

All implementations of @racket[gen:sequence] are required to implement this method.}

@defproc[(nth [seq sequence?] [index exact-nonnegative-integer?]) any/c]{
Retrieves the element within @racket[seq] at @racket[index].

If @racket[seq] also implements @racket[gen:countable] @emph{and} is @racket[known-finite?], bounds
checking will automatically be provided, and a @racket[exn:fail:contract] error will be raised if
@racket[index] is out of range.

This method is optional if an implementation of @racket[first] is provided.}

@defproc[(reverse [seq sequence?]) sequence?]{
Returns a new sequence with all the elements of @racket[seq], but in reverse order. If @racket[seq] is
infinite, this may not terminate.}

@subsubsection[#:tag "sequence-functions"]{Derived Functions}

@defproc[(last [seq sequence?]) any/c]{
Gets the last element of @racket[seq]. If @racket[seq] is infinite, this may not terminate.}

@defproc[(apply [proc procedure?] [arg any/c] ... [args sequence?] [#:<kw> kw-arg any/c] ...) any]{
The same as @racket[base:apply] but with support for any sequence as the final argument instead of
only lists. Just like in @racket[base:apply], @racket[#:<kw>] stands for any keyword.}

@defproc[(append [seq sequence?] ...) any/c]{
Returns a new @emph{lazy sequence} with all the values of the @racket[seq] arguments concatenated, in
order.

In many cases, it may be preferably to use @racket[extend] or @racket[extend*], which may also provide
better performance, especially for homogenous sequence types.}

@defproc[(take [n exact-nonnegative-integer?] [seq sequence?]) sequence?]{
Returns a new @emph{lazy sequence} that contains the first @racket[n] elements of @racket[seq].}

@defproc[(drop [n exact-nonnegative-integer?] [seq sequence?]) sequence?]{
Returns a new sequence that contains all @emph{except} the first @racket[n] elements of @racket[seq].}

@defproc[(subsequence [seq sequence?]
                      [start exact-nonnegative-integer?]
                      [end exact-nonnegative-integer?]) sequence?]{
Returns a new sequence containing the elements of @racket[seq] from @racket[start], inclusive, to
@racket[end], exclusive. Equivalent to @racket[(take (- end start) (drop start seq))].}

@defproc[(filter [pred (any/c . -> . any/c)] [seq sequence?]) sequence?]{
Returns a new @emph{lazy sequence} containing all the elements of @racket[seq] for which @racket[pred]
applied to them produces a non-@racket[#f] value.}

@defproc[(map [proc procedure?] [seq sequence?] ...+) sequence?]{
Returns a new @emph{lazy sequence} consisting of the results of applying @racket[proc] to the elements
of the provided @racket[seq] arguments. The @racket[proc] procedure must take as many arguments as
@racket[seq] arguments are provided. If more than one @racket[seq] is provided, they must all be of
the same length.}

@defproc[(foldl [proc procedure?] [init any/c] [seq sequence?] ...+) any/c]{
Continually applies @racket[proc] over the elements in the provided @racket[seq] arguments, passing
the result of each application to the subsequent invokation of @racket[proc]. The @racket[proc]
procedure must accept @italic{n}+1 arguments where @italic{n} is the number of @racket[seq] arguments
provided. If more than one @racket[seq] is provided, they must all be of the same length.

Unlike @racket[base:foldl], the accumulator argument is always provided to @racket[proc] @emph{first},
not last.}

@deftogether[(@defproc[(second [coll collection?]) any/c]
              @defproc[(third [coll collection?]) any/c]
              @defproc[(fourth [coll collection?]) any/c]
              @defproc[(fifth [coll collection?]) any/c]
              @defproc[(sixth [coll collection?]) any/c]
              @defproc[(seventh [coll collection?]) any/c]
              @defproc[(eighth [coll collection?]) any/c]
              @defproc[(ninth [coll collection?]) any/c]
              @defproc[(tenth [coll collection?]) any/c])]{
A set of helper functions for accessing elements of @racket[coll] implemented in terms of
@racket[nth]. A random-access implementation of @racket[nth] will make these random-access as well.}

@defproc[(in [seq sequence?]) stream?]{
When used as a procedure, converts @racket[seq] into a lazy @reftech{stream}. This function is
primarily intended to be used directly in a @racket[for] clause, in which case the sequence will
be iterated directly without any conversion taking place.}

@deftogether[(@defform[(for/sequence (for-clause ...) body-or-break ... body)]
              @defform[(for*/sequence (for-clause ...) body-or-break ... body)])]{
Both forms iterate like @racket[for], but the results of the @racket[body] expressions are collected
into a @emph{lazy sequence}. This means that the body of the loop isn't actually evaluated until the
sequence is used, so any side-effects performed will be delayed until the sequence is forced.

The @racket[for*/sequence] form is the same as @racket[for/sequence] but with the implicit nesting
behavior of @racket[for*].}

@defproc[(sequence->list [seq sequence?]) list?]{
Converts any sequence to a list. Equivalent to @racket[(reverse (extend '() seq))].

If @racket[seq] is infinite, then this function will not terminate, and it will infinitely allocate
memory until it is exhausted.}

@section{General-Purpose Interfaces}

@subsection{Countable Collections}

Lots of data structures may be considered @deftech{countable}—that is, they have a discrete number of
elements. The @racket[gen:countable] interface only provides a single function, @racket[length].

@defthing[gen:countable any/c]{

A @reftech{generic interface} that defines two functions, @racket[length], which accepts a
single argument and returns the number of elements contained within the collection, and
@racket[known-finite?], which provides a pessimistic check for finite collections.

The following built-in datatypes have implementations for @racket[gen:countable]:

@itemlist[
  @item{@reftech{lists}}
  @item{@reftech{vectors}}
  @item{@reftech{strings}}
  @item{@reftech{byte strings}}
  @item{@reftech{hash tables}}
  @item{@reftech{sets}}
  @item{@reftech{dictionaries}}
  @item{@reftech{streams}}]

For @reftech{streams}, if the argument is infinite, then @racket[length] does not terminate.

@(examples
  #:eval (evaluator)
  (length (range 20))
  (length #(λ))
  (length "Hello!")
  (length (set 1 2 3 4 5))
  (struct wrapped-collection (value)
    #:methods gen:countable
    [(define/generic -length length)
     (define (length w)
       (-length (wrapped-collection-value w)))])
  (length (wrapped-collection (hash 'a "b" 'c "d"))))}

@defproc[(countable? [v any/c]) boolean?]{

A predicate that identifies if @racket[v] is @tech{countable}.}

@defproc[(length [countable countable?]) exact-nonnegative-integer?]{
Returns the number of discrete elements contained by @racket[countable]. If @racket[countable] is
infinite, then this function does not terminate.}

@defproc[(known-finite? [countable countable?]) boolean?]{
If this function returns @racket[#t], then @racket[countable] @emph{must} be finite, and therefore,
@racket[length] must terminate. If this function returns @racket[#f], then no additional information
is gained: @racket[countable] could be either finite or infinite.

If no implementation for @racket[known-finite?] is provided, it will always return @racket[#f].}

@subsection{Indexable Collections}

Data structures are @deftech{indexable} if they provide any sort of indexed data.

@defthing[gen:indexable any/c]{

A @reftech{generic interface} that defines exactly one function, @racket[ref], which accepts an
instance of @racket[gen:indexable] and an index.

@margin-note{
Be careful when using @racket[ref] with @reftech{dictionaries}. While they @emph{are}
@tech{indexable}, using @racket[ref] with @reftech{association lists} will use @racket[list-ref]
rather than @racket[dict-ref].}

All @tech{generic sequences} are also @tech{indexable}, so implementations of @racket[gen:sequence] do
@emph{not} need to implement @racket[gen:indexable]. Additionally, mutable @reftech{hash tables},
mutable @reftech{vectors}, and @reftech{dictionaries} are also indexable.

@(examples
  #:eval (evaluator)
  (ref '(a b c) 1)
  (ref (hash 'foo "bar") 'foo))}

@defproc[(indexable? [v any/c]) boolean?]{

A predicate that identifies if @racket[v] is @tech{indexable}.}

@defproc[(ref [collection indexable?] [index any/c]) any]{

Returns the value associated with the provided @racket[index] for the given @racket[collection].}
