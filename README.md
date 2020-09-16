# Purely Functional Data Structures

This repository contains a variety of purely functional data
structures.

Mostly, the data structures come from "Purely Functional Data
Structures" by Chris Okasaki.

This library provides multiple implementations of various data
structures so that you can pick the one that fits your needs.  If you
don't want to think too much about it, the `PFDS.SHCL.IO` package
exposes convenience constructors for common data structures (namely
sets, maps, sequences, and heaps).  Unless documented otherwise, you
may assume that all data structures provided by this library may be
used persistently.

If you are inclined to use reader macros, `PFDS.SHCL.IO` contains a
named readtable with macros for sets, maps, and sequences.

## Data Structures

### Maps

#### `RED-BLACK-MAP`

- `MAKE-RED-BLACK-MAP`: `O(n log(n))`
- `WITH-ENTRY`: `O(log(n))`
- `WITHOUT-ENTRY`: `O(log(n))`
- `LOOKUP-ENTRY`: `O(log(n))`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

#### `WEIGHT-BALANCED-MAP`

- `MAKE-WEIGHT-BALANCED-MAP`: `O(n log(n))`
- `WITH-ENTRY`: `O(log(n))`
- `WITHOUT-ENTRY`: `O(log(n))`
- `LOOKUP-ENTRY`: `O(log(n))`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

### Sets

#### `RED-BLACK-SET`

- `MAKE-RED-BLACK-SET`: `O(n log(n))`
- `WITH-MEMBER`: `O(log(n))`
- `WITHOUT-MEMBER`: `O(log(n))`
- `IS-MEMBER`: `O(log(n))`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

#### `WEIGHT-BALANCED-SET`

- `MAKE-WEIGHT-BALANCED-SET`: `O(n log(n))`
- `WITH-MEMBER`: `O(log(n))`
- `WITHOUT-MEMBER`: `O(log(n))`
- `IS-MEMBER`: `O(log(n))`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

### Queues

#### `BATCHED-QUEUE`

- `MAKE-BATCHED-QUEUE`: `O(n)`
- `WITH-LAST`, `WITHOUT-FIRST`: `O(1)` amortized
- `PEEK-FIRST`: `O(1)`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

Note: this queue *is* purely functional, but the asymptotic bounds
given above assume the queue is not used persistently.  If you require
persistence, consider using `BANKERS-QUEUE`.

#### `BANKERS-QUEUE`

- `MAKE-BANKERS-QUEUE`: `O(n)`
- `WITH-LAST`, `WITHOUT-FIRST`: `O(1)` amortized
- `PEEK-FIRST`: `O(1)`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

Unlike `BATCHED-QUEUE`, the bounds given above remain true even if the
queue is used persistently.  However, the constant factors `WITH-LAST`
and `WITHOUT-FIRST` are much higher.  If you don't need persistence,
consider using `BATCHED-QUEUE`.

### Deques

#### `BATCHED-DEQUE`

- `MAKE-BATCHED-DEQUE`: `O(n)`
- `WITH-LAST`, `WITHOUT-LAST`, `WITH-FIRST`, `WITHOUT-FIRST`: `O(1)` amortized
- `PEEK-FIRST`: `O(1)`
- `PEEK-LAST`: `O(1)`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

Note: this queue *is* purely functional, but the asymptotic bounds
given above assume the queue is not used persistently.  Also be aware
that this data structure requires more consing than `BATCHED-QUEUE`,
so if you only need a queue then you should consider using
`BATCHED-QUEUE` instead.

#### `WEIGHT-BALANCED-SEQUENCE`

- `MAKE-WEIGHT-BALANCED-SEQUENCE`: `O(n log(n))`
- `WITH-LAST`: `O(log(n))`
- `WITHOUT-LAST`: `O(log(n))`
- `WITH-FIRST`: `O(log(n))`
- `WITHOUT-FIRST`: `O(log(n))`
- `PEEK-FIRST`: `O(log(n))`
- `PEEK-LAST`: `O(log(n))`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

### Heaps

#### `LEFTIST-HEAP`

- `MAKE-LEFTIST-HEAP`: `O(n)`
- `HEAP-TOP`: `O(1)`
- `WITH-MEMBER`: `O(log(n))`
- `WITHOUT-HEAP-TOP`: `O(log(n))`
- `MERGE-HEAPS`: `O(log(n))`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

#### `BINOMIAL-HEAP`

- `MAKE-BINOMIAL-HEAP`: `O(n)`
- `HEAP-TOP`: `O(log(n))`
- `WITH-MEMBER`: `O(log(n))`
- `WITHOUT-HEAP-TOP`: `O(log(n))`
- `MERGE-HEAPS`: `O(log(n))`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

#### `SPLAY-HEAP`

- `MAKE-SPLAY-HEAP`: `O(n log(n))` amortized
- `HEAP-TOP`: `O(1)`
- `WITH-MEMBER`: `O(log(n))` amortized
- `WITHOUT-HEAP-TOP`: `O(log(n))` amortized
- `MERGE-HEAPS`: `O(n)` amortized
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

Note: this heap *is* purely functional, but the asymptotic bounds
given above assume the heap is not used persistently.

#### `PAIRING-HEAP`

- `MAKE-PAIRING-HEAP`: `O(n)`
- `HEAP-TOP`: `O(1)`
- `WITH-MEMBER`: `O(1)`
- `WITHOUT-HEAP-TOP`: `O(log(n))`
- `MERGE-HEAPS`: `O(1)`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

Note: this heap *is* purely functional, but the asymptotic bounds
given above assume the heap is not used persistently.

### Sequences

#### `PERSISTENT-VECTOR`

- `HEAD`: `O(log(n))`
- `WITH-HEAD`: `O(log(n))`
- `TAIL`: `O(log(n))`
- `LOOKUP-ENTRY`: `O(log(n))`
- `WITH-ENTRY`: `O(log(n))`
- `WITHOUT-ENTRY`: `O(n log(n))` in general, `O(log(n))` if removing the last entry
- `CONCATENATE-SEQUENCES`: `O(n log(n))`
- `SEQUENCE-INSERT`: `O(n log(n))` in general, `O(log(n))` if adding to the end
- `SUBSEQUENCE`: `O(n log(n))`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

This data structure is similar to an adjustable vector.  Pushing or
popping at the end of the vector is cheap.  Looking up an index is
cheap.  Operating on the middle of the vector is expensive.

Note: the logarithmic base for the bounds given above is very large.

#### `WEIGHT-BALANCED-SEQUENCE`

- `HEAD`: `O(log(n))`
- `WITH-HEAD`: `O(log(n))`
- `TAIL`: `O(log(n))`
- `LOOKUP-ENTRY`: `O(log(n))`
- `WITH-ENTRY`: `O(log(n))`
- `WITHOUT-ENTRY`: `O(log(n))`
- `CONCATENATE-SEQUENCES`: `O(log(n))` (see below)
- `SEQUENCE-INSERT`: `O(log(n))`
- `SUBSEQUENCE`: `O(log(n))`
- `IS-EMPTY`: `O(1)`
- `SIZE`: `O(1)`

Compared to `PERSISTENT-VECTOR`, this sequence type permits efficient
manipulation anywhere in the sequence.

Interestingly, concatenating weight-balanced sequences is `O(1)` when
the sequences are a similar length.  Weight balanced trees ensure that
the "heavier" child weighs no more than some constant times the weight
of the "lighter" child.  As long as the sequence lengths satisfy the
balance criteria, concatenation is `O(1)`.  Concatenation can be done
by simply recursively descending down the spine of the longer sequence
until we find a node of similar size to the smaller sequence. At that
point, the sequences can be joined in `O(1)` time.  We'll still need
to rebalance the tree on our way back up, but each individual balance
operation only takes `O(1)` time.  Asymptotic bounds with multiple
variables are a bit hand-wavy, but the running time should look
something like `O(log(n/m))`.  `n` is the length of the longer
sequence, and `m` is the length of the shorter sequence.

The implementation for weight-balanced sequences attempts to
efficiently store character types.  When possible, character types are
collected together to form short strings.  The optimization is
defeated if the sequence contains non-character types, but the effect
is only local to the portion of the sequence where the non-character
is found.  More distant portions of the sequence will still optimize
the storage of characters.

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
objects typically perform much worse than they ought to.  All
asymptotic bounds given assume no `:UNEQUAL` objects.  Avoiding
uneqaulity is very important.  As such, `COMPARE` establishes
orderings that you may not expect.  For example, `COMPARE` establishes
an ordering between things that are mutually `=`, such as `1`, `1.0`,
`(COMPLEX 1)`, and `(COMPLEX 1.0)`.  Its probably best if you think of
`COMPARE` as the ordering equivalent of a hash function.  The results
aren't necessarily meaningful, but they do have useful properties for
specific use cases.  If you want to have a meaningful ordering, you
can simply define your own comparator function.  The
`PFDS.SHCL.IO/UTILITY/COMPARE` package exports comparators for many
common types to help facilitate writing your own comparator.

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
`EQL` would be distinct keys in a map.  That's unlikely to be what
most people want.  As a concession for practicality, the `COMPARE`
function ignores the mutability of data types provided by Common Lisp.

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

Unfortunately, `DEFSTRUCT` is a bit annoying to work with.
- Uninitialized slot values are undefined.
- For immutable structs, every slot needs to be declared read-only
  individually.
- Working with immutable structs is an exercise in frustration.  When
  you want to clone a struct but with a change, you need to list out
  all the slots of the strict!
- There's not a meta-object protocol for structures.  If you want to
  introspect a class struct (e.g. to simplify the above) you need to
  parse the `DEFSTRUCT` form.  Unfortunately, correctly parsing a
  `DEFSTRUCT` form is tricky!

These aren't big problems on their own, but they become really
annoying when you take a hard-line stance about using immutable
structs.  Luckily, a little bit of metaprogramming goes a long way.
This project defines a rudimentary MOP for structures, and then
leverages that MOP to create convenient abstractions for immutable
structures.  E.g. for structs using the `IMMUTABLE-STRUCTURE`
metaclass, `:COPIER` functions accept keyword arguments for overriding
slot values while creating the new object.

### Avoid generic functions

Generic functions are great for interfaces where you want to accept
objects of different types.  Unfortunately, they impose a performance
penalty that is completely unnecessary when you know the types of the
arguments being passed into the function.  With [inlined generic
functions](https://github.com/guicho271828/inlined-generic-function),
you can avoid almost all of the cost in some cases.  Unfortunately,
this is dependent on sophisticated compiler optimizations, and its all
too easy to end up in a case where the compiler can't eliminate the
type dispatch logic.  Since core data structures ought to be fast,
this project avoids the use of generic functions to implement the
internals of its data structures.

You can argue that this is probably an instance of premature
optimization.  You'd be right.  I didn't actually profile anything
before I made this decision.  The performance concerns I outlined
above seemed reasonable to me, but mostly I just thought it would be
more interesting if I restricted myself to using generic functions
sparingly.

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

### `IMPURE-SPLAY-MAP` and `IMPURE-SPLAY-SET`

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
