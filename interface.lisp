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

(defpackage :pfds.shcl.io/interface
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects)
  (:export
   #:comparable
   #:compare-objects

   #:debugable
   #:check-invariants
   #:print-graphviz

   #:collection
   #:iterator
   #:empty
   #:map-members
   #:size
   #:with-member
   #:decompose
   #:member-type
   #:is-empty
   #:for-each
   #:to-list

   #:ordered-collection
   #:comparator

   #:indexed-collection
   #:with-entry
   #:lookup-entry
   #:without-entry
   #:for-each-entry
   #:map-entries

   #:ordered-map

   #:set
   #:without-member
   #:is-member

   #:ordered-set

   #:stack
   #:with-top
   #:without-top
   #:peek-top

   #:queue
   #:with-back
   #:without-front
   #:peek-front

   #:deque
   #:with-front
   #:without-back
   #:peek-back

   #:priority-queue
   #:meld
   #:peek-front
   #:without-front

   #:sequence
   #:insert
   #:join
   #:subsequence
   #:with-first
   #:without-first
   #:with-last
   #:without-last

   #:do-each
   #:graphviz
   #:next-graphviz-id

   #:define-interface
   #:interface-functions
   #:interface-supers
   #:declare-interface-conformance
   #:interfaces-implemented
   #:classes-implementing-interface))
(in-package :pfds.shcl.io/interface)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun interface-superclasses (interface-name)
    (get interface-name 'interface-superclasses))

  (defun (setf interface-superclasses) (new-value interface-name)
    (setf (get interface-name 'interface-superclasses) new-value))

  (defun interface-required-functions (interface-name)
    (get interface-name 'interface-required-functions))

  (defun (setf interface-required-functions) (new-value interface-name)
    (setf (get interface-name 'interface-required-functions) new-value))

  (defun interface-optional-functions (interface-name)
    (get interface-name 'interface-optional-functions))

  (defun (setf interface-optional-functions) (new-value interface-name)
    (setf (get interface-name 'interface-optional-functions) new-value)))

(defun %do-interface-inheritance-tree-f (interface-stack visited function)
  (let ((interface-name (car interface-stack)))
    (when (gethash interface-name visited)
      (return-from %do-interface-inheritance-tree-f))

    (when (position interface-name (cdr interface-stack))
      (error "Cyclic inheritance detected: ~W" interface-stack))

    (let ((supers (interface-superclasses interface-name)))
      (dolist (super supers)
        (%do-interface-inheritance-tree-f (cons super interface-stack) visited function))
      (funcall function interface-name)
      (setf (gethash interface-name visited) t))))

(defun do-interface-inheritance-tree-f (interface-name function)
  (%do-interface-inheritance-tree-f (list interface-name) (make-hash-table) function))

(defmacro do-interface-inheritance-tree ((interface-name seed-interface) &body body)
  `(do-interface-inheritance-tree-f ,seed-interface (lambda (,interface-name) ,@body)))

(defun interface-supers (interface-name)
  (interface-superclasses interface-name))

(defun interface-functions (interface-name &key (include-inherited-functions t))
  (let ((visited (make-hash-table)))
    (labels
        ((visit-interface (interface)
           (let ((required-functions (interface-required-functions interface))
                 (optional-functions (interface-optional-functions interface)))
             (dolist (function required-functions)
               (setf (gethash function visited) :required))
             (dolist (function optional-functions)
               (let ((value (gethash function visited)))
                 (case value
                   (:required)
                   (:optional)
                   ((nil)
                    (setf (gethash function visited) :optional))))))))
      (if include-inherited-functions
          (do-interface-inheritance-tree (interface interface-name)
            (visit-interface interface))
          (visit-interface interface-name)))
    (loop :for function-name :being :the :hash-keys :of visited
            :using (:hash-value strength) :collect
          (cons function-name strength))))

(defmacro define-interface (name &body clauses)
  (let (required-functions
        optional-functions
        superclasses)
    (loop :for thing :in clauses :do
      (etypecase thing
        (symbol
         (push thing required-functions))
        (cons
         (destructuring-bind (indicator value) thing
           (check-type value symbol)
           (ecase indicator
             (:required
              (push value required-functions))
             (:optional
              (push value optional-functions))
             (:inherit
              (push value superclasses)))))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (interface-required-functions ',name) ',required-functions)
         (setf (interface-optional-functions ',name) ',optional-functions)
         (setf (interface-superclasses ',name) ',superclasses))
       ',name)))

(defun add-interface-conformance (class-name interface-names)
  (let ((class-conformance-table (get class-name 'interface-conformance)))
    (unless class-conformance-table
      (setf class-conformance-table (make-hash-table))
      (setf (get class-name 'interface-conformance) class-conformance-table))

    (dolist (interface-name interface-names)
      (do-interface-inheritance-tree (interface-name interface-name)
        (setf (gethash interface-name class-conformance-table) t)

        (let ((interface-conformers-table (get interface-name 'interface-conformers)))
          (unless interface-conformers-table
            (setf interface-conformers-table (make-hash-table))
            (setf (get interface-name 'interface-conformers) interface-conformers-table))
          (setf (gethash class-name interface-conformers-table) t))))))

(defmacro declare-interface-conformance (class-name &rest interface-names)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (add-interface-conformance ',class-name ',interface-names)))

(defun interfaces-implemented (class-name)
  (let ((table (or (get class-name 'interface-conformance) (make-hash-table))))
    (loop :for interface :being :the :hash-keys :of table :collect interface)))

(defun classes-implementing-interface (interface-name)
  (let ((table (or (get interface-name 'interface-conformers) (make-hash-table))))
    (loop :for class-name :being :the :hash-keys :of table :collect class-name)))

(defgeneric check-invariants (collection)
  (:documentation
   "Ensure that the given collection is internally self-consistent."))

(defmethod check-invariants (collection)
  ;; looks fine to me!
  )

(defun next-graphviz-id (id-vendor)
  (incf (symbol-value id-vendor)))

(defgeneric print-graphviz (object stream id-vendor))

(defun graphviz (object &optional (stream *standard-output*))
  (let ((id-vendor (gensym "ID-VENDOR")))
    (setf (symbol-value id-vendor) 0)
    (format stream "digraph {~%")
    (print-graphviz object stream id-vendor)
    (format stream "}~%"))
  (values))

(defgeneric iterator (collection)
  (:documentation
   "Return a function which, when repeatedly invoked, returns the
elements of the given collection.

When the iterator is done it will return two nil values.

The order that the elements are returned in depends on the type of the
collection."))

(defgeneric empty (collection)
  (:documentation
   "Return an empty collection of the same type as the given one."))

(defgeneric map-members (collection function)
  (:documentation
   "Return an associative collection where the values have been updated.

Note: If your function returns a value eql to the one it was given,
the collection may be able to avoid some consing.  In the extreme
case, the resulting collection may be eql to the initial one!
Naturally, if you intend to always return the input value, you'd be
better served by the `FOR-EACH' generic function."))

(defgeneric size (collection)
  (:documentation
   "Returns the number of elements in the given collection."))

(defgeneric with-member (collection item)
  (:documentation
   "Add an object to the given collection.

What it means to add an entry to a collection depends on the
collection's type."))

(defgeneric decompose (collection)
  (:documentation
   "Split an object out of the given collection.

If the collection is empty, they returns the input collection followed
by two nil values.  If the collection is non-empty, it returns the
collection, the removed element, and a non-nil value."))

(defgeneric member-type (collection)
  (:documentation
   "Returns the type of member objects."))

(defmethod member-type (collection)
  t)

(defgeneric to-list (collection)
  (:documentation
   "Convert the given collection into a list."))

(defmethod to-list (collection)
  (let ((builder (make-impure-list-builder)))
    (labels
        ((visit (value)
           (impure-list-builder-add builder value)))
      (for-each collection #'visit))
    (impure-list-builder-extract builder)))

(defgeneric is-empty (collection)
  (:documentation
   "Returns non-nil if the given collection has no elements."))

(defmethod is-empty (collection)
  (zerop (size collection)))

(defgeneric for-each (collection function)
  (:documentation
   "Call the given function on each object in the collection.

In the case of maps, the function will receive a cons cell containing
a key and a value."))

(defmethod for-each (collection function)
  (let ((iterator (iterator collection)))
    (loop
      (multiple-value-bind (value valid-p) (funcall iterator)
        (unless valid-p
          (return))
        (funcall function value)))))

(defmacro do-each ((value collection &optional result) &body body)
  "Iterate over the values contained in the collection."
  `(block nil
     (for-each ,collection (lambda (,value) ,@body))
     ,result))

(defgeneric comparator (collection)
  (:documentation
   "Returns the comparator the collection is using."))

(defgeneric with-entry (collection key value)
  (:documentation
   "Return a collection where the given key/value pair has been added."))

(defgeneric lookup-entry (collection key)
  (:documentation
   "Return the value associated with the given key in the collection.

If the value doesn't exist in the collection, this returns two nil values.
If the value does exist, it returns the stored value and a non-nil
value."))

(defgeneric without-entry (collection key)
  (:documentation
   "Return a collection where the entry for the given key has been removed."))

(defgeneric for-each-entry (collection function)
  (:documentation
   "Call the given function on each pair in the collection.

Each time the function is called, it receives a key and a value.  The
key argument represents a value that can be passed to functions such
as `LOOKUP-ENTRY', `WITH-ENTRY', and `WITHOUT-ENTRY'.  The value is
the object the collection stores in association with the key.

This generic function is only valid for containers that support
`LOOKUP-ENTRY' and similar functions."))

(defgeneric map-entries (collection function)
  (:documentation
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
`LOOKUP-ENTRY' and similar functions."))

(defgeneric without-member (collection object)
  (:documentation
   "Return a new collection where the given object has been removed."))

(defgeneric is-member (collection object)
  (:documentation
   "Returns non-nil if the given object is a member of the collection."))

(defgeneric with-top (stack object)
  (:documentation
   "Returns a stack where the given object has been placed on the
top."))

(defgeneric without-top (stack)
  (:documentation
   "Return a stack where the topmost element has been removed.

If the stack is empty, this returns an empty stack and two nil values.
If the stack is non-empty, it returns the updated stack, the removed
value, and a non-nil value."))

(defgeneric peek-top (stack)
  (:documentation
   "Retrieve the object from the top of the stack.

If the stack is empty, this returns two nil values.  If the stack is
non-empty, it returns the first object in the stack and a non-nil
value."))

(defgeneric with-back (queue object)
  (:documentation
   "Return a queue with an object added to the back."))

(defgeneric without-front (queue)
  (:documentation
   "Return a queue where the frontmost element has been removed.

If the queue is empty, this returns an empty queue and two nil values.
If the queue is non-empty, it returns the updated queue, the removed
value, and a non-nil value."))

(defgeneric peek-front (queue)
  (:documentation
   "Retrieve the object at the front of the queue.

If the queue is empty, this returns two nil values.  If the queue is
non-empty, it returns the first object in the queue and a non-nil
value."))

(defgeneric with-front (deque object)
  (:documentation
   "Return a deque where the given object added to the front."))

(defgeneric without-back (deque)
  (:documentation
   "Return a queue where the backmost element has been removed.

If the queue is empty, this returns an empty queue and two nil values.
If the queue is non-empty, it returns the updated queue, the removed
value, and a non-nil value."))

(defgeneric peek-back (deque)
  (:documentation
   "Retrieve the object from the back of the deque.

If the deque is empty, this returns two nil values.  If the deque is
non-empty, it returns the first object in the deque and a non-nil
value."))

(defgeneric meld (first second)
  (:documentation
   "Return a priority queue that contains all the elements in the
given priority queues."))

(defgeneric insert (sequence before-index object)
  (:documentation
   "Return a new sequence where the given object is inserted before
the given index."))

(defgeneric join (left right)
  (:documentation
   "Return a new sequence where the given sequences are joined
together (i.e. concatenated)."))

(defgeneric subsequence (sequence min max)
  (:documentation
   "Return a new sequence which contains elements in the range [min,
max)."))

(defgeneric with-first (sequence object)
  (:documentation
   "Return a new sequence where the given object has been inserted
before index 0."))

(defgeneric without-first (sequence)
  (:documentation
   "Return a sequence where the first (i.e. 0th index) element has been removed.

If the sequence is empty, this returns an empty sequence and two nil
values.  If the sequence is non-empty, it returns the updated
sequence, the removed value, and a non-nil value."))

(defgeneric with-last (sequence object)
  (:documentation
   "Return a new sequence where the given object has been inserted
at the end."))

(defgeneric without-last (sequence)
  (:documentation
   "Return a sequence where the last (i.e. largest index) element has been removed.

If the sequence is empty, this returns an empty sequence and two nil
values.  If the sequence is non-empty, it returns the updated
sequence, the removed value, and a non-nil value."))

(define-interface comparable
  compare-objects)

(define-interface debugable
  check-invariants
  print-graphviz)

(define-interface collection
  (:inherit debugable)
  (:inherit comparable)
  iterator
  empty
  map-members
  size
  with-member
  decompose
  (:optional member-type)
  (:optional is-empty)
  (:optional for-each)
  (:optional to-list))

(define-interface ordered-collection
  (:inherit collection)
  comparator)

(define-interface indexed-collection
  (:inherit collection)
  with-entry
  lookup-entry
  without-entry
  for-each-entry
  map-entries)

(define-interface ordered-map
  (:inherit indexed-collection)
  (:inherit ordered-collection))

(define-interface set
  (:inherit collection)
  with-member
  without-member
  is-member)

(define-interface ordered-set
  (:inherit ordered-collection)
  (:inherit set))

(define-interface stack
  (:inherit collection)
  with-top
  without-top
  peek-top)

(define-interface queue
  (:inherit collection)
  with-back
  without-front
  peek-front)

(define-interface deque
  (:inherit queue)
  (:inherit stack)
  with-front
  without-back
  peek-back)

(define-interface priority-queue
  (:inherit ordered-collection)
  with-member
  meld
  peek-front
  without-front)

(define-interface sequence
  (:inherit indexed-collection)
  (:inherit deque)
  insert
  join
  subsequence
  with-first
  without-first
  with-last
  without-last)
