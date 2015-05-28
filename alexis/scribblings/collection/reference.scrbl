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
              racket/stream
              racket/string)
   scribble/eval
   "../private/utils.rkt")

@title[#:tag "collections-api"]{API Documentation}

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
 @item{@emph{immutable} @reftech{hash sets}}
 @item{@emph{immutable} @reftech{dictionaries}}]

@(coll-examples
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
provided.

@(coll-examples
  (conj '() 'a)
  (conj '(1 2) 3)
  (conj #(1 2) 3)
  (conj (hash) '(a . b)))}

@defproc[(extend [a collection?] [b sequence?]) collection?]{
Returns a new collection with all the items in both @racket[a] and @racket[b], and the result is the
same kind of collection as @racket[a].

If @racket[conj] is implemented but not @racket[extend], an implementation will automatically be
provided.

@(coll-examples
  (extend '(1 2) '(3 4))
  (extend '() #(1 2 3 4))
  (extend #() '(1 2 3 4))
  (extend (hash) (set '(a . b) '(b . c))))}

@subsubsection[#:tag "collection-functions"]{Derived Functions}

@defproc[(conj* [coll collection?] [item any/c] ...) collection?]{
Repeatedly calls @racket[conj] for each @racket[item] provided, in order.

@(coll-examples
  (conj* '() 1 2 3 4))}

@defproc[(extend* [base collection?] [extension sequence?] ...) collection?]{
Repeatedly calls @racket[extend] for each @racket[extension] provided, in order.

@(coll-examples
  (extend* '() #(1 2) (stream 3 4) (set 5 6)))}

@subsection{Sequences}

@defthing[gen:sequence any/c]{

A @reftech{generic interface} that represents any ordered sequence of values. The
@racket[gen:sequence] interface provides the @racket[empty?], @racket[first], @racket[rest],
@racket[nth], @racket[set-nth], @racket[update-nth], @racket[reverse], and @racket[random-access?]
functions.

The following built-in datatypes have implementations for @racket[gen:sequence]:

@itemlist[
 @item{@reftech{lists}}
 @item{@emph{immutable} @reftech{hash tables}}
 @item{@emph{immutable} @reftech{vectors}}
 @item{@emph{immutable} @reftech{hash sets}}
 @item{@emph{immutable} @reftech{dictionaries}}
 @item{@reftech{streams}}]

@(coll-examples
  (extend (set) (map + '(1 2 3) #(4 5 6))))}

@defproc[(sequence? [v any/c]) boolean?]{
A predicate that determines if @racket[v] is a @tech{generic sequence}.}

@subsubsection[#:tag "sequence-methods"]{Generic Methods}

@defproc[(empty? [seq sequence?]) boolean?]{
Determines if @racket[seq] has no values.

All implementations of @racket[gen:sequence] are required to implement this method, unless they also
implement @racket[gen:countable].

@(coll-examples
  (empty? '())
  (empty? (stream))
  (empty? #())
  (empty? '(a b c)))}

@defproc[(first [seq (and/c sequence? (not/c empty?))]) any/c]{
Retrieves the first values in @racket[seq].

This method is optional if an implementation of @racket[nth] is provided.

@(coll-examples
  (first '(1 2 3))
  (first (set 'a 'b 'c))
  (first (hash 'a 'b 'c 'd)))}

@defproc[(rest [seq (and/c sequence? (not/c empty?))]) any/c]{
Retrieves a new sequence which represents all but the first value in @racket[seq].

All implementations of @racket[gen:sequence] are required to implement this method.

@(coll-examples
  (rest '(1 2 3))
  (rest (set 'a 'b 'c))
  (rest (hash 'a 'b 'c 'd))
  (extend (hash) (rest (hash 'a 'b 'c 'd))))}

@defproc[(nth [seq sequence?] [index exact-nonnegative-integer?]) any/c]{
Retrieves the element within @racket[seq] at @racket[index].

If @racket[seq] also implements @racket[gen:countable] @emph{and} is @racket[known-finite?], bounds
checking will automatically be provided, and a @racket[exn:fail:contract] error will be raised if
@racket[index] is out of range.

This method is optional if an implementation of @racket[first] is provided.

@(coll-examples
  (nth '(1 2 3) 1)
  (nth #(1 2 3) 2))}

@defproc[(set-nth [seq sequence?] [index exact-nonnegative-integer?] [value any/c]) sequence?]{
Performs a functional update and returns a new sequence with the same elements as @racket[seq], except
the element at @racket[index] is replaced with @racket[value].

@(coll-examples
  (set-nth '(1 2 3) 1 'b))}

@defproc[(update-nth [seq sequence?] [index exact-nonnegative-integer?] [proc (any/c . -> . any/c)])
         sequence?]{
Like @racket[set-nth], but instead of supplying the value to be replaced directly, the @racket[proc]
procedure is applied to the old value at @racket[index] to produce its replacement.

@(coll-examples
  (update-nth '(1 2 3) 1 (λ (n) (+ 10 n))))}

@defproc[(reverse [seq sequence?]) sequence?]{
Returns a new sequence with all the elements of @racket[seq], but in reverse order. If @racket[seq] is
infinite, this may not terminate.

@(coll-examples
  (reverse '(1 2 3))
  (reverse #(1 2 3))
  (extend #() (reverse #(1 2 3))))}

@defproc[(random-access? [seq sequence?]) boolean?]{
Provides a way for sequence implementations to convey whether or not they are random access. If no
implementation is provided, the default implementation always returns @racket[#f].

This can be used as a heuristic to determine what sort of algorithm to use when operating over generic
sequences. For example, if a sequence is determined to be random access, the default implementation
for @racket[update-nth] will use @racket[nth] and @racket[set-nth]. Otherwise, it will lazily loop.}

@subsubsection[#:tag "sequence-functions"]{Derived Functions}

@defproc[(last [seq sequence?]) any/c]{
Gets the last element of @racket[seq]. If @racket[seq] is infinite, this may not terminate.

@(coll-examples
  (last '(1 2 3))
  (last (set 'a 'b 'c))
  (last (hash 'a 'b 'c 'd)))}

@defproc[(apply [proc procedure?] [arg any/c] ... [args sequence?] [#:<kw> kw-arg any/c] ...) any]{
The same as @racket[base:apply] but with support for any sequence as the final argument instead of
only lists. Just like in @racket[base:apply], @racket[#:<kw>] stands for any keyword.

@(coll-examples
  (apply + #(1 1 1))
  (apply + (set 1 1 1))
  (apply string-replace #:all? #f "foo" #("o" "a")))}

@defproc[(append [seq sequence?] ...) any/c]{
Returns a new @emph{lazy sequence} with all the values of the @racket[seq] arguments concatenated, in
order.

In many cases, it may be preferably to use @racket[extend] or @racket[extend*], which may also provide
better performance, especially for homogenous sequence types.

@(coll-examples
  (append '(1 2) '(3 4))
  (sequence->list (append '(1 2) '(3 4)))
  (sequence->list (append (hash 'a 'b) (set 'c 'd))))}

@defproc[(take [n exact-nonnegative-integer?] [seq sequence?]) sequence?]{
Returns a new @emph{lazy sequence} that contains the first @racket[n] elements of @racket[seq].

@(coll-examples
  (sequence->list (take 10 (in-naturals))))}

@defproc[(drop [n exact-nonnegative-integer?] [seq sequence?]) sequence?]{
Returns a new sequence that contains all @emph{except} the first @racket[n] elements of @racket[seq].

@(coll-examples
  (drop 5 (range 10)))}

@defproc[(subsequence [seq sequence?]
                      [start exact-nonnegative-integer?]
                      [end exact-nonnegative-integer?]) sequence?]{
Returns a new sequence containing the elements of @racket[seq] from @racket[start], inclusive, to
@racket[end], exclusive. Equivalent to @racket[(take (- end start) (drop start seq))].

@(coll-examples
  (sequence->list (subsequence (in-naturals) 10 20)))}

@defproc[(subsequence* [seq sequence?]
                       [start exact-nonnegative-integer?]
                       [len exact-nonnegative-integer?]) sequence?]{
Like @racket[subsequence], but instead of specifying the end index, @racket[len] specifies the length
of the resulting sequence. Equivalent to @racket[(take len (drop start seq))] and
@racket[(subsequence seq start (+ start len))].

@(coll-examples
  (sequence->list (subsequence* (in-naturals) 20 5)))}

@defproc[(filter [pred (any/c . -> . any/c)] [seq sequence?]) sequence?]{
Returns a new @emph{lazy sequence} containing all the elements of @racket[seq] for which @racket[pred]
applied to them produces a non-@racket[#f] value.

@(coll-examples
  (filter odd? '(1 2 3 4 5))
  (sequence->list (filter odd? '(1 2 3 4 5))))}

@defproc[(map [proc procedure?] [seq sequence?] ...+) sequence?]{
Returns a new @emph{lazy sequence} consisting of the results of applying @racket[proc] to the elements
of the provided @racket[seq] arguments. The @racket[proc] procedure must take as many arguments as
@racket[seq] arguments are provided. If more than one @racket[seq] is provided, they must all be of
the same length.

@(coll-examples
  (map add1 '(10 20 30))
  (sequence->list (map add1 '(10 20 30)))
  (sequence->list (map + '(5 10 15) #(3 6 9)))
  (define fibs (stream-cons 1 (stream-cons 1 (map + fibs (rest fibs)))))
  (sequence->list (take 15 fibs)))}

@defproc[(foldl [proc procedure?] [init any/c] [seq sequence?] ...+) any/c]{
Continually applies @racket[proc] over the elements in the provided @racket[seq] arguments, passing
the result of each application to the subsequent invokation of @racket[proc]. The @racket[proc]
procedure must accept @italic{n}+1 arguments where @italic{n} is the number of @racket[seq] arguments
provided. If more than one @racket[seq] is provided, they must all be of the same length.

Unlike @racket[base:foldl], the accumulator argument is always provided to @racket[proc] @emph{first},
not last.

@(coll-examples
  (foldl cons null (set 1 2 3 4))
  (foldl (λ (a b) (cons b a)) null (set 1 2 3 4)))}

@defproc[(for-each [proc procedure?] [seq sequence?] ...+) void?]{
Applies @racket[proc] over the @racket[seq] arguments just like @racket[map], but does so strictly and
does not return a sequence. Instead, it simply returns @|void-const|.}

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
@racket[nth]. A random-access implementation of @racket[nth] will make these random-access as well.

@(coll-examples
  (second (in-naturals))
  (third (in-naturals))
  (fourth (in-naturals)))}

@defproc[(in [seq sequence?]) stream?]{
When used as a procedure, converts @racket[seq] into a lazy @reftech{stream}. This function is
primarily intended to be used directly in a @racket[for] clause, in which case the sequence will
be iterated directly without any conversion taking place.

@(coll-examples
  (in '(1 2 3 4))
  (for ([e (in (filter even? (set 1 2 3 4 5 6)))])
    (displayln e)))}

@deftogether[(@defform[(for/sequence (for-clause ...) body-or-break ... body)]
              @defform[(for*/sequence (for-clause ...) body-or-break ... body)])]{
Both forms iterate like @racket[for], but the results of the @racket[body] expressions are collected
into a @emph{lazy sequence}. This means that the body of the loop isn't actually evaluated until the
sequence is used, so any side-effects performed will be delayed until the sequence is forced.

The @racket[for*/sequence] form is the same as @racket[for/sequence] but with the implicit nesting
behavior of @racket[for*].

@(coll-examples
  (extend
   (set)
   (for/sequence ([i (in-range 10)])
     (* i i))))}

@defproc[(sequence->list [seq sequence?]) list?]{
Converts any sequence to a list. Equivalent to @racket[(reverse (extend '() seq))].

If @racket[seq] is infinite, then this function will not terminate, and it will infinitely allocate
memory until it is exhausted.

@(coll-examples
  (sequence->list #(1 2 3))
  (sequence->list (hash 'a 'b 1 2 "foo" "bar")))}

@defproc[(sequence->string [seq (sequenceof char?)]) (and/c string? sequence?)]{
Converts @racket[seq], which must contain only @reftech{characters}, to an immutable
@reftech{string}.}

@defproc[(sequence->bytes [seq (sequenceof byte?)]) (and/c bytes? sequence?)]{
Converts @racket[seq], which must contain only
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{bytes}, to an immutable @reftech{byte string}.}

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

@(coll-examples
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

If no implementation for @racket[known-finite?] is provided, it will always return @racket[#f].

@(coll-examples
  (known-finite? #(a b c))
  (known-finite? (range 10))
  (known-finite? (in-naturals)))}

@subsection{Indexable Collections}

Data structures are @deftech{indexable} if they provide any sort of indexed data.

@defthing[gen:indexable any/c]{

A @reftech{generic interface} that defines exactly one function, @racket[ref], which accepts an
instance of @racket[gen:indexable] and an index.

@margin-note{
Be careful when using @racket[ref] with @tech{generic sequences}. If numeric indexing is your
intention, use @racket[nth] instead, since @racket[ref] and @racket[nth] may have different behaviors
on the same sequence. Notably, @racket[ref] on @reftech{association lists} uses @racket[dict-ref], not
@racket[list-ref].}

All @tech{generic sequences} are also @tech{indexable}, so implementations of @racket[gen:sequence] do
@emph{not} need to implement @racket[gen:indexable] if they provide simply key/value mappings based on
index. Additionally, mutable @reftech{hash tables}, mutable @reftech{vectors},
and @reftech{dictionaries} are also indexable.

@(coll-examples
  (ref '(a b c) 1)
  (ref (hash 'foo "bar") 'foo)
  (ref '((1 . 2) (3 . 4)) 1))}

@defproc[(indexable? [v any/c]) boolean?]{

A predicate that identifies if @racket[v] is @tech{indexable}.}

@defproc[(ref [collection indexable?] [index any/c]) any]{

Returns the value associated with the provided @racket[index] for the given @racket[collection].}

@subsection{Contracts on Collections}

@defproc[(sequenceof [ctc contract?] [#:chaperone? chaperone? any/c #f]) contract?]{
Produces a @reftech{contract} that recognizes sequences and ensures their elements all match the
@racket[ctc] contract. When a @racket[sequenceof] contract is applied to a sequence, the result is not
@racket[eq?] to its input.

If @racket[chaperone?] is non-@racket[#f], then the result will always be a
@racket[chaperone-contract?], and @racket[ctc] @emph{must} also be a @racket[chaperone-contract?]. If
@racket[chaperone?] is @racket[#f], the result will always be a simple @racket[contract?].

For most sequence types, when a @racket[sequenceof] contract is applied to a sequence, the result is
always @racket[equal?] to its input. However, for a small set of sequences, such as @reftech{hash
tables}, @reftech{strings}, and @reftech{byte strings}, the result will be an entirely disparate type
of sequence. This behavior is only supported for non-chaperone contracts, so if @racket[chaperone?] is
non-@racket[#f], then those sequences will not be permitted by the contract.}
