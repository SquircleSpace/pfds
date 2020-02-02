# Purely Functional Data Structures

This repository contains a variety of purely functional data
structures.

For now, it only contains data structures described in the book
"Purely Functional Data Structures" by Chris Okasaki.  The
implementations have been modified to feel more natural in Common
Lisp.  Many data structures from the book are not yet implemented.

In the future, this library may contain other data purely functional
structures as well.

## Data Structures

### Maps

#### `RED-BLACK-MAP`

- `MAKE-RED-BLACK-MAP`: `O(n log(n))`
- `WITH-ENTRY`: `O(log(n))`
- `WITHOUT-ENTRY`: `O(log(n))`
- `LOOKUP-ENTRY`: `O(log(n))`
- `IS-EMPTY`: `O(1)`

### Sets

#### `RED-BLACK-SET`

- `MAKE-RED-BLACK-SET`: `O(n log(n))`
- `WITH-MEMBER`: `O(log(n))`
- `WITHOUT-MEMBER`: `O(log(n))`
- `IS-MEMBER`: `O(log(n))`
- `IS-EMPTY`: `O(1)`

### Queues

#### `BATCHED-QUEUE`

- `MAKE-BATCHED-QUEUE`: `O(n)`
- `WITH-LAST`, `WITHOUT-FIRST`: `O(1)` amortized
- `PEEK-FIRST`: `O(1)`
- `IS-EMPTY`: `O(1)`

Note: this queue *is* purely functional, but the asymptotic bounds
given above assume the queue is not used persistently.

### Deques

#### `BATCHED-DEQUE`

- `MAKE-BATCHED-DEQUE`: `O(n)`
- `WITH-LAST`, `WITHOUT-LAST`, `WITH-FIRST`, `WITHOUT-FIRST`: `O(1)` amortized
- `PEEK-FIRST`: `O(1)`
- `PEEK-LAST`: `O(1)`
- `IS-EMPTY`: `O(1)`

Note: this queue *is* purely functional, but the asymptotic bounds
given above assume the queue is not used persistently.  Also be aware
that this data structure requires more consing than `BATCHED-QUEUE`,
so if you only need a queue then you should consider using
`BATCHED-QUEUE` instead.

### Heaps

#### `LEFTIST-HEAP`

- `MAKE-LEFTIST-HEAP`: `O(n)`
- `HEAP-TOP`: `O(1)`
- `WITH-MEMBER`: `O(log(n))`
- `WITHOUT-HEAP-TOP`: `O(log(n))`
- `MERGE-HEAPS`: `O(log(n))`
- `IS-EMPTY`: `O(1)`

#### `BINOMIAL-HEAP`

- `MAKE-BINOMIAL-HEAP`: `O(n)`
- `HEAP-TOP`: `O(log(n))`
- `WITH-MEMBER`: `O(log(n))`
- `WITHOUT-HEAP-TOP`: `O(log(n))`
- `MERGE-HEAPS`: `O(log(n))`
- `IS-EMPTY`: `O(1)`

#### `SPLAY-HEAP`

- `MAKE-SPLAY-HEAP`: `O(n log(n))` amortized
- `HEAP-TOP`: `O(1)`
- `WITH-MEMBER`: `O(log(n))` amortized
- `WITHOUT-HEAP-TOP`: `O(log(n))` amortized
- `MERGE-HEAPS`: `O(n)` amortized
- `IS-EMPTY`: `O(1)`

Note: this heap *is* purely functional, but the asymptotic bounds
given above assume the heap is not used persistently.

## Design decisions

### Dependencies

This project has a minimal set of dependencies.  Thus far, the core
data structures have no external dependencies other than Common Lisp
itself.  This allows the data structures to be used in almost any
project.

### `COMPARE`

Many functional data structures rely on establishing a relative
ordering between objects.  All such data structures accept a
comparator function at construction time.  The function should accept
two objects and return either `:LESS`, `:GREATER`, `:EQUAL`, or
`:UNEQUAL`.  If you are familiar with the `FSet` library, this should
sound familiar.  This library provides a comparison function for your
convenience: `COMPARE`.

The provided `COMPARE` generic function tries to avoid returning
`:UNEQUAL` as much as possible.  Data structures containing `:UNEQUAL`
objects typically perform much worse than they ought to.  So, avoiding
uneqaulity is very important.  As such, `COMPARE` establishes
orderings that you may not expect.  For example, `COMPARE` establishes
an ordering between things that are mutually `=`, such as `1`, `1.0`,
`(COMPLEX 1)`, and `(COMPLEX 1.0)`.  Its probably best if you think of
`COMPARE` as the ordering equivalent of a hash function.  The results
aren't necessarily meaningful, but they do have useful properties for
specific use cases.  If you want to have a meaningful ordering, you
can simply define your own comparator function.  The
`PFDS.SHCL.IO/COMPARE` package exports comparators for many common
types to help facilitate writing your own comparator.

Note that `COMPARE` assumes that instances of types provided by Common
Lisp are immutable.  You mustn't mutate objects in ways that change
their ordering after adding them to an ordering-dependent data
structure.  Its probably best to just avoid mutating them at all.  A
principled approach would be to return `:UNEQUAL` when comparing
un-`EQL` instances of a mutable type.  Unfortunately, that would
result in very common use cases becoming very inefficient.  For
example, its very common to use symbols and strings as members of a
set or keys in a map.  If all un-`EQL` symbols and strings were
`:UNEQUAL` then the performance for that use case would plummet.  Even
if we established an ordering somehow (e.g. using pointer address in
implementations that allow that) strings that are `EQUAL` but not
`EQL` would distinct keys in a map.  That's unlikely to be what most
people want. As a concession for practicality, the `COMPARE` function
ignores the mutability of data types provided by Common Lisp.

For user-defined mutable classes, you are strongly encouraged to use
the following `COMPARE` method.
```
(defmethod compare-objects ((left my-type) (right my-type))
  (if (eql left right)
      :equal
      (unequalify (compare-reals (sxhash left) (sxhash right)))))
```
This ensures that the ordering invariants of data structures are not
violated by mutation while also minimizing the number of `:UNEQUAL`
entries.

### `DEFSTRUCT` vs `DEFCLASS`

For pure data structures, this library always uses `DEFSTRUCT` instead
of `DEFCLASS`.  Generally, `DEFCLASS` is the go-to way to define an
aggregate data type.  It offers a number of features that make
development much easier.  Ordinarily, these would be considered good
things.  Unfortunately, these features often run counter to the goal
of having immutable instances.

For example, a class definition can be updated at any time.  This has
the potential to change the contents of an instance of the class.  The
whole goal of a purely function data structure is to forbid mutation,
and so class redefinition cannot be permitted.  Similarly, the
initialization methods used by `DEFCLASS` are problematic.  Mutation
is an essential part of how instances are initialized.

For purely functional data structures, less is more.  By restricting
ourselves to `DEFSTRUCT`, we get some substantial benefits.  We're
able to communicate to the compiler that the instance is immutable.  A
smart compiler can leverage this information to emit more efficient
code.  Furthermore, it allows the compiler to keep us honest.  It is
much more difficult to accidentally leak side effects when you can't
have them!

## Impure data structures

Some algorithms are best implemented with impure data structures.  The
following data structures were useful when implementing pure data
structures.  They are provided for your convenience.

### `IMPURE-QUEUE`

The `IMPURE-QUEUE` is a queue that uses a circular buffer.  The queue
will automatically grow or shrink the buffer as the number of elements
changes.

### `IMPURE-LIST-BUILDER`

The `IMPURE-LIST-BUILDER` is an object that allows you to construct a
list in FIFO order.  Instead of repeatedly pushing elements onto a
list and then reversing the result, you can instead add elements to
the list builder and then extract the result.

#### `IMPURE-SPLAY-MAP` and `IMPURE-SPLAY-SET`

Note that splay trees are troublesome in a purely functional context.
Even queries against the tree need to mutate the tree in order to
maintain balance.  Furthermore, they have poor asymptotic running time
when used persistently.  The mutability issue can be hidden by the
implementation using a lock, but the issue with persistence is
unavoidable.

That's a shame, because splay trees are super fun!  Due to the
overwhelming fun-ness of splay trees, this library provides an impure
implementation of splay sets and maps.  Under the hood, these are
implemented using purely functional trees.  So, you can at least feel
warm and fuzzy about that... assuming consing makes you feel warm and
fuzzy.

- `MAKE-IMPURE-SPLAY-SET`: `O(n log(n))`
- `IMPURE-SPLAY-SET-IS-EMPTY`: `O(1)`
- `IMPURE-SPLAY-SET-INSERT`, `IMPURE-SPLAY-SET-REMOVE`, `IMPURE-SPLAY-SET-IS-MEMBER`: `O(log(n))` amortized
- `IMPURE-SPLAY-SET-REMOVE-ALL`: `O(1)`

- `MAKE-IMPURE-SPLAY-MAP`: `O(n log(n))`
- `IMPURE-SPLAY-MAP-IS-EMPTY`: `O(1)`
- `IMPURE-SPLAY-MAP-INSERT`, `IMPURE-SPLAY-MAP-REMOVE`, `IMPURE-SPLAY-MAP-LOOKUP`: `O(log(n))` amortized
- `IMPURE-SPLAY-MAP-REMOVE-ALL`: `O(1)`
