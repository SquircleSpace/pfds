;; Copyright 2019 Ada Avery
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(uiop:define-package :pfds.shcl.io/implementation/interface
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/utility/interface
   #:define-interface
   #:interface-get
   #:interface-functions
   #:define-interface-function-invoker
   #:define-interface-function
   #:declaim-signature
   #:interface-function-lambda-list
   #:single-specializer
   #:double-specializer)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc #:get+)
  (:import-from :pfds.shcl.io/utility/specialization
   #:define-specializable-function)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare #:compare-objects)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/forwarding
   #:make-forwarding-lambda
   #:make-forwarding-method)
  (:import-from :alexandria)
  (:export
   #:collection-to-list

   #:<<comparable>>
   #:compare
   #:i-compare
   #:g-compare

   #:<<debugable>>
   #:check-invariants
   #:i-check-invariants
   #:g-check-invariants
   #:print-graphviz
   #:i-print-graphviz
   #:g-print-graphviz
   #:next-graphviz-id
   #:i-next-graphviz-id
   #:g-next-graphviz-id
   #:graphviz
   #:graphviz-quote
   #:representative-empty
   #:i-representative-empty
   #:g-representative-empty

   #:<<collection>>
   #:iterator
   #:i-iterator
   #:g-iterator
   #:empty
   #:i-empty
   #:g-empty
   #:map-members
   #:i-map-members
   #:g-map-members
   #:size
   #:i-size
   #:g-size
   #:with-member
   #:i-with-member
   #:g-with-member
   #:decompose
   #:i-decompose
   #:g-decompose
   #:member-type
   #:i-member-type
   #:g-member-type
   #:is-empty
   #:i-is-empty
   #:g-is-empty
   #:for-each
   #:i-for-each
   #:g-for-each
   #:to-list
   #:i-to-list
   #:g-to-list

   #:<<ordered-collection>>
   #:comparator
   #:i-comparator
   #:g-comparator

   #:<<indexed-collection>>
   #:with-entry
   #:i-with-entry
   #:g-with-entry
   #:lookup-entry
   #:i-lookup-entry
   #:g-lookup-entry
   #:without-entry
   #:i-without-entry
   #:g-without-entry
   #:for-each-entry
   #:i-for-each-entry
   #:g-for-each-entry
   #:map-entries
   #:i-map-entries
   #:g-map-entries

   #:<<map>>

   #:<<ordered-map>>
   #:make-ordered-map
   #:i-make-ordered-map

   #:<<set>>
   #:with-member
   #:i-with-member
   #:g-with-member
   #:without-member
   #:i-without-member
   #:g-without-member
   #:is-member
   #:i-is-member
   #:g-is-member

   #:<<ordered-set>>
   #:make-ordered-set
   #:i-make-ordered-set

   #:<<stack>>
   #:with-top
   #:i-with-top
   #:g-with-top
   #:without-top
   #:i-without-top
   #:g-without-top
   #:peek-top
   #:i-peek-top
   #:g-peek-top
   #:reverse
   #:i-reverse
   #:g-reverse
   #:make-stack
   #:i-make-stack

   #:<<queue>>
   #:with-back
   #:i-with-back
   #:g-with-back
   #:without-front
   #:i-without-front
   #:g-without-front
   #:peek-front
   #:i-peek-front
   #:g-peek-front
   #:make-queue
   #:i-make-queue

   #:<<deque>>
   #:with-front
   #:i-with-front
   #:g-with-front
   #:without-back
   #:i-without-back
   #:g-without-back
   #:peek-back
   #:i-peek-back
   #:g-peek-back
   #:make-deque
   #:i-make-deque

   #:<<priority-queue>>
   #:with-member
   #:i-with-member
   #:g-with-member
   #:meld
   #:i-meld
   #:g-meld
   #:peek-front
   #:i-peek-front
   #:g-peek-front
   #:without-front
   #:i-without-front
   #:g-without-front
   #:make-priority-queue
   #:i-make-priority-queue

   #:<<seq>>
   #:insert
   #:i-insert
   #:g-insert
   #:join
   #:i-join
   #:g-join
   #:subsequence
   #:i-subsequence
   #:g-subsequence
   #:make-seq
   #:i-make-seq))
(in-package :pfds.shcl.io/implementation/interface)

(define-interface-function compare (left right)
  :documentation
  "Compare and determine ordering between two objects of the same type.

This function can return :LESS, :GREATER, :EQUAL, or :UNEQUAL."
  :define-generic nil
  :generic-name compare-objects
  :method-generator double-specializer)

(define-interface-function check-invariants (collection)
  :documentation
  "Ensure that the given collection is internally self-consistent.")

(defun next-graphviz-id (id-vendor)
  (incf (symbol-value id-vendor)))

(define-interface-function print-graphviz (object stream id-vendor)
  :documentation
  "Print the given object's graphviz representation to the stream.")

(defun graphviz (object &optional (stream *standard-output*) interface)
  (let ((id-vendor (gensym "ID-VENDOR")))
    (setf (symbol-value id-vendor) 0)
    (format stream "digraph {~%")
    (if interface
        (i-print-graphviz interface object stream id-vendor)
        (g-print-graphviz object stream id-vendor))
    (format stream "}~%"))
  (values))

(defun graphviz-quote (object)
  (let ((string (if (stringp object)
                    object
                    (format nil "~A" object))))
    (with-output-to-string (stream)
      (write-char #\" stream)
      (loop :for char :across string :do
        (case char
          ;; Escape backslashes
          (#\\ (write-string "\\\\" stream))
          ;; Escape quotes
          (#\" (write-string "\\\"" stream))
          (otherwise (write-char char stream))))
      (write-char #\" stream))))

(define-interface-function representative-empty ()
  :documentation
  "Returns an object that represents a potential empty value for the
collection.")

(define-interface-function iterator (collection)
  :documentation
  "Return a function which, when repeatedly invoked, returns the
elements of the given collection.

When the iterator is done it will return two nil values.

The order that the elements are returned in depends on the type of the
collection.")

(define-interface-function empty (collection)
  :documentation
  "Return an empty collection of the same type as the given one.")

(define-interface-function map-members (collection function)
  :documentation
  "Return an associative collection where the values have been updated.

Note: If your function returns a value eql to the one it was given,
the collection may be able to avoid some consing.  In the extreme
case, the resulting collection may be eql to the initial one!
Naturally, if you intend to always return the input value, you'd be
better served by the `FOR-EACH' generic function.")

(define-interface-function size (collection)
  :documentation
  "Returns the number of elements in the given collection.")

(define-interface-function with-member (collection item)
  :documentation
  "Add an object to the given collection.

What it means to add an entry to a collection depends on the
collection's type.")

(define-interface-function decompose (collection)
  :documentation
  "Split an object out of the given collection.

If the collection is empty, they returns the input collection followed
by two nil values.  If the collection is non-empty, it returns the
collection, the removed element, and a non-nil value.")

(define-interface-function member-type (collection)
  :documentation
  "Returns the type of member objects.")

(declaim (inline always-t))
(defun always-t (collection)
  (declare (ignore collection))
  t)

(define-interface-function to-list (collection)
  :documentation
  "Convert the given collection into a list.")

(define-interface-function is-empty (collection)
  :documentation
  "Returns non-nil if the given collection has no elements.")

(define-specializable-function collection-is-empty (<interface>) (collection)
  (zerop (i-size <interface> collection)))

(define-interface-function for-each (collection function)
  :documentation
  "Call the given function on each object in the collection.

In the case of maps, the function will receive a cons cell containing
a key and a value.")

(define-specializable-function collection-to-list (<interface>) (collection)
  (let ((builder (make-impure-list-builder)))
    (labels
        ((visit (value)
           (impure-list-builder-add builder value)))
      (i-for-each <interface> collection #'visit))
    (impure-list-builder-extract builder)))

(define-specializable-function collection-for-each (<interface>) (collection function)
  (let ((iterator (i-iterator <interface> collection)))
    (loop
      (multiple-value-bind (value valid-p) (funcall iterator)
        (unless valid-p
          (return))
        (funcall function value)))))

(define-interface-function comparator (collection)
  :documentation
  "Returns the comparator the collection is using.")

(define-interface-function with-entry (collection key value)
  :documentation
  "Return a collection where the given key/value pair has been added.")

(define-interface-function lookup-entry (collection key)
  :documentation
  "Return the value associated with the given key in the collection.

If the value doesn't exist in the collection, this returns two nil values.
If the value does exist, it returns the stored value and a non-nil
value.")

(define-interface-function without-entry (collection key)
  :documentation
  "Return a collection where the entry for the given key has been removed.")

(define-interface-function for-each-entry (collection function)
  :documentation
  "Call the given function on each pair in the collection.

Each time the function is called, it receives a key and a value.  The
key argument represents a value that can be passed to functions such
as `LOOKUP-ENTRY', `WITH-ENTRY', and `WITHOUT-ENTRY'.  The value is
the object the collection stores in association with the key.

This generic function is only valid for containers that support
`LOOKUP-ENTRY' and similar functions.")

(define-interface-function map-entries (collection function)
  :documentation
  "Return an associative collection where the values have been updated.

Each time the function is called, it receives a key and a value.  The
key argument represents an object that can be passed to functions such
as `LOOKUP-ENTRY', `WITH-ENTRY', and `WITHOUT-ENTRY'.  The value is
the object the collection stores in association with the key.  The
function is expected to return a new value for the collection to
store.  This function returns the collection where all values have
been updated accordingly.

Note: If your function returns a value eql to the one it was given,
the collection may be able to avoid some consing.  In the extreme
case, the resulting collection may be eql to the initial one!
Naturally, if you intend to always return the input value, you'd be
better served by the `FOR-EACH-ENTRY' generic function.

This generic function is only valid for containers that support
`LOOKUP-ENTRY' and similar functions.")

(define-interface-function make-ordered-map (&key comparator alist plist)
  :documentation
  "Returns a fresh associative map collection."
  :generic-name nil
  :define-generic nil)

(define-interface-function without-member (collection object)
  :documentation
  "Return a new collection where the given object has been removed.")

(define-interface-function is-member (collection object)
  :documentation
  "Returns non-nil if the given object is a member of the collection.")

(define-interface-function make-ordered-set (&key comparator items)
  :documentation
  "Returns a fresh set."
  :generic-name nil
  :define-generic nil)

(define-interface-function with-top (stack object)
  :documentation
  "Returns a stack where the given object has been placed on the
top.")

(define-interface-function without-top (stack)
  :documentation
  "Return a stack where the topmost element has been removed.

If the stack is empty, this returns an empty stack and two nil values.
If the stack is non-empty, it returns the updated stack, the removed
value, and a non-nil value.")

(define-interface-function peek-top (stack)
  :documentation
  "Retrieve the object from the top of the stack.

If the stack is empty, this returns two nil values.  If the stack is
non-empty, it returns the first object in the stack and a non-nil
value.")

(define-interface-function reverse (stack)
  :documentation
  "Returns a fresh collection that has all the elements of the input
but in reverse order.")

(define-interface-function make-stack (&key items)
  :documentation
  "Return a stack with the given items in it."
  :generic-name nil
  :define-generic nil)

(define-interface-function with-back (queue object)
  :documentation
  "Return a queue with an object added to the back.")

(define-interface-function without-front (queue)
  :documentation
  "Return a queue where the frontmost element has been removed.

If the queue is empty, this returns an empty queue and two nil values.
If the queue is non-empty, it returns the updated queue, the removed
value, and a non-nil value.")

(define-interface-function make-priority-queue (&key comparator items)
  :documentation
  "Returns a priority queue containing the given items."
  :generic-name nil
  :define-generic nil)

(define-interface-function peek-front (queue)
  :documentation
  "Retrieve the object at the front of the queue.

If the queue is empty, this returns two nil values.  If the queue is
non-empty, it returns the first object in the queue and a non-nil
value.")

(define-interface-function make-queue (&key items)
  :documentation
  "Produce a queue that contains the given items.

The items are inserted into the queue in the same order they appear in
in the input sequence."
  :generic-name nil
  :define-generic nil)

(define-interface-function with-front (deque object)
  :documentation
  "Return a deque where the given object added to the front.")

(define-interface-function without-back (deque)
  :documentation
  "Return a queue where the backmost element has been removed.

If the queue is empty, this returns an empty queue and two nil values.
If the queue is non-empty, it returns the updated queue, the removed
value, and a non-nil value.")

(define-interface-function peek-back (deque)
  :documentation
  "Retrieve the object from the back of the deque.

If the deque is empty, this returns two nil values.  If the deque is
non-empty, it returns the first object in the deque and a non-nil
value.")

(define-interface-function make-deque (&key items)
  :documentation
  "Returns a deque with the given items in it."
  :generic-name nil
  :define-generic nil)

(define-interface-function meld (first second)
  :documentation
  "Return a priority queue that contains all the elements in the
given priority queues."
  :method-generator double-specializer)

(define-interface-function insert (sequence before-index object)
  :documentation
  "Return a new sequence where the given object is inserted before
the given index.")

(define-interface-function join (left right)
  :documentation
  "Return a new sequence where the given sequences are joined
together (i.e. concatenated)."
  :method-generator double-specializer)

(define-interface-function subsequence (sequence min max)
  :documentation
  "Return a new sequence which contains elements in the range [min,
max).")

(define-interface-function make-seq (&key items)
  :documentation
  "Returns a sequence containing the given items."
  :define-generic nil
  :generic-name nil)

(define-interface <<comparable>> ()
  compare)

(define-interface <<debugable>> ()
  check-invariants
  print-graphviz
  representative-empty)

(define-interface <<collection>> (<<debugable>> <<comparable>>)
  iterator
  empty
  map-members
  size
  with-member
  decompose
  ;; TODO: optional is kind of broken right now.  Optional functions
  ;; don't get a pointer to the interface and can't call other
  ;; interface functions
  (:optional member-type always-t)
  is-empty
  ;; (:optional is-empty collection-is-empty)
  for-each
  ;; (:optional for-each collection-for-each)
  to-list
  ;; (:optional to-list collection-to-list)
  )

(define-interface <<ordered-collection>> (<<collection>>)
  comparator)

(define-interface <<indexed-collection>> (<<collection>>)
  with-entry
  lookup-entry
  without-entry
  for-each-entry
  map-entries)

(define-interface <<map>> (<<indexed-collection>> <<collection>>))

(define-interface <<ordered-map>> (<<map>> <<ordered-collection>>)
  make-ordered-map)

(define-interface <<set>> (<<collection>>)
  with-member
  without-member
  is-member)

(define-interface <<ordered-set>> (<<set>> <<ordered-collection>>)
  make-ordered-set)

(define-interface <<stack>> (<<collection>>)
  with-top
  without-top
  peek-top
  reverse
  make-stack)

(define-interface <<queue>> (<<collection>>)
  with-back
  without-front
  peek-front
  make-queue)

(define-interface <<deque>> (<<queue>> <<stack>>)
  with-front
  without-back
  peek-back
  make-deque)

(define-interface <<priority-queue>> (<<ordered-collection>>)
  with-member
  meld
  peek-front
  without-front
  make-priority-queue)

(define-interface <<seq>> (<<indexed-collection>> <<deque>>)
  insert
  join
  subsequence
  make-seq)
