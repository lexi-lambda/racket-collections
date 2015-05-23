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

@title[#:tag "collections-intro"]{Introduction}

The @racketmodname[alexis/collection] library attempts to provide a suitable generic interface for
interacting with all of Racket's collections through a uniform interface. That said, in doing so, its
approach is something of a departure from how some of the functions from @racketmodname[racket/base]
operate.

Virtually all of the functions provided by @racketmodname[alexis/collection] are
@emph{collection-agnostic}, so they will operate on any type of collection with consistent behavior.
This is in stark contrast to @racketmodname[racket/base]'s library, including functions with distinct
behavior for different kinds of collections (e.g. @racket[list-ref], @racket[vector-ref],
@racket[string-ref], @etc).

As an example, @racket[ref] can operate on all kinds of sequences with reasonable behavior for all of
them:

@(coll-interaction
  (ref '(1 2 3) 1)
  (ref #(1 2 3) 1)
  (ref (in-naturals) 5)
  (ref (hash 'a 'b 'c 'd) 'a))

However, it also means that some of the functions provided by @racketmodname[alexis/collection] have
different behavior from their @racketmodname[racket/base] equivalents. When calling a function on a
collection in @racketmodname[racket/base], there is a guarantee on the type of collection recieved as
a result. With generic collections, there is often no such guarantee.

For example, consider reversing a list. This is a simple operation, and it performs as one might
expect.

@(coll-interaction
  (reverse '(1 2 3)))

But what about reversing a vector? The same strategy would require allocating a new vector, which
would be unnecessarily slow. Instead, a different kind of sequence is returned.

@(coll-interaction
  (reverse #(1 2 3)))

The only guarantee is that the result must be a sequence, but otherwise, it can be almost anything.
Fortunately, in the majority of cases, this is irrelevant, which is the point of generic collections:
you don't need to worry about what @emph{kind} of collection you are dealing with, since the behavior
is still what one would expect.

@(coll-interaction
  (first (reverse #(1 2 3))))

This also permits one of the other changes from @racketmodname[racket/base]—a few of the collections
operations are @emph{lazy}, in that they return @emph{lazy sequences}. In many cases, these lazy
sequences are Racket @reftech{streams}, but not always. For example, @racket[map] is lazy.

@(coll-interaction
  (map add1 '(10 20 30)))

Sometimes, of course, it is useful to convert a collection into a particular representation. Usually,
this can be done using @racket[extend], which takes a particular sequence and returns a new sequence
with the values from a different sequence added. For example, we can put the results from the example
above into a vector:

@(coll-interaction
  (extend #() (map add1 '(10 20 30))))

The implementation of @racket[extend] uses the primitive collection operator, @racket[conj]. It is
much like @racket[cons] for lists in that it adds a single value to a collection. However, it also
makes no guarantees about what @emph{order} the new elements are placed in. For example, @racket[conj]
prepends to lists but appends to vectors.

@(coll-interaction
  (conj '(1 2 3) 4)
  (conj #(1 2 3) 4))

This permits efficient implementation of @racket[conj] on a per-collection basis. It does mean that
using @racket[extend] on lists will reverse the input sequence, which is probably not desired in the
majority of cases. For that purpose, @racket[sequence->list] is provided, which is equivalent to
@racket[reverse] combined with @racket[extend].

@(coll-interaction
  (extend '() (map add1 '(10 20 30)))
  (sequence->list (map add1 '(10 20 30))))

A few other functions are lazy, such as @racket[filter] and @racket[append], though functions that do
not return sequences cannot be lazy, such as @racket[foldl], so they are still strict.

The existence of a generic interface also allows the various for loop sequence operations, such as
@racket[in-list], @racket[in-vector], @racket[in-stream], @etc, can be collected into a single
operator, simply called @racket[in]. When used as an ordinary function, it simply returns a lazy
sequence equivalent to its input. However, when used in a @racket[for] clause, it expands into a more
efficient form which iterates over the sequence directly.

@(coll-interaction
  (in #(1 2 3))
  (for ([e (in #(1 2 3))])
    (displayln e)))

Additionally, a @racket[for/sequence] form is provided, which operates similarly to @racket[for/list],
but it returns a @emph{lazy} sequence, so it can even operate on infinite sequences.

@(coll-interaction
  (for/sequence ([e (in-naturals)])
    (* e e)))

For a full list of all functions which support generic sequences, see the @secref["collections-api"].
