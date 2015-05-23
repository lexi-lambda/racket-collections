#lang scribble/manual

@(require
   "private/utils.rkt")

@title{Generic Collections}

@defmodule[alexis/collection]

This provides a set of @reftech{generic interfaces} for built-in Racket collections to create a
unified interface for working with Racket data structures.
@seclink["structures" #:doc '(lib "scribblings/reference/reference.scrbl")]{User-defined structures}
may also implement the collections API to provide implementations for additional datatypes.

This collection provides a number of bindings that override bindings in @racketmodname[racket/base],
some with slightly different semantics in addition to support for multiple kinds of collections. For
this reason, this @emph{may} not be a drop-in replacement for existing code.

@local-table-of-contents[]

@include-section["collection/introduction.scrbl"]
@include-section["collection/examples.scrbl"]
@include-section["collection/reference.scrbl"]