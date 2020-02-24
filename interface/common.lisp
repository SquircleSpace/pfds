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

(defpackage :pfds.shcl.io/interface/common
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:export
   #:is-empty #:empty #:with-member
   #:with-entry #:lookup-entry #:without-entry
   #:to-list
   #:iterator
   #:for-each
   #:for-each-kv
   #:do-sequence
   #:size
   #:check-invariants
   #:graphviz
   #:print-graphviz
   #:next-graphviz-id
   #:define-interface))
(in-package :pfds.shcl.io/interface/common)

(defgeneric to-list (collection)
  (:documentation
   "Convert the given collection into a list."))

(defgeneric iterator (collection))

(defgeneric is-empty (collection)
  (:documentation
   "Returns non-nil if the given collection has no elements."))

(defgeneric empty (collection)
  (:documentation
   "Return an empty collection of the same type as the given one."))

(defgeneric for-each (collection function)
  (:documentation
   "Call the given function on each object in the collection.

In the case of maps, the function will receive a cons cell containing
a key and a value."))

(defgeneric for-each-kv (collection function)
  (:documentation
   "Call the given function on each pair in the collection.

Each time the function is called, it receives a key and a value.  The
key argument represents a value that can be passed to functions such
as `LOOKUP-ENTRY', `WITH-ENTRY', and `WITHOUT-ENTRY'.  The value is
the object the collection stores in association with the key.

This generic function is only valid for containers that support
`LOOKUP-ENTRY' and similar functions."))

(defmacro do-sequence ((value collection &optional result) &body body)
  "Iterate over the values contained in the collection."
  (let ((rest (gensym "REST")))
    `(block nil
       (for-each ,collection (lambda (,value &rest ,rest)
                               (declare (ignore ,rest))
                               ,@body))
       ,result)))

(defgeneric size (collection)
  (:documentation
   "Returns the number of elements in the given collection."))

(defmethod to-list (collection)
  (let ((builder (make-impure-list-builder)))
    (labels
        ((visit (value)
           (impure-list-builder-add builder value)))
      (for-each collection #'visit))
    (impure-list-builder-extract builder)))

(defgeneric with-member (collection item)
  (:documentation
   "Add an object to the given collection."))

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

(defmacro define-interface (name &body functions)
  `(progn
     ,@(loop
         :for thing :in functions
         :collect
         (etypecase thing
           (symbol `',thing)
           (cons
            (if (eq (car thing) 'defgeneric)
                thing
                (error "Invalid interface")))))
     ',name))

(defmethod is-empty ((list list))
  (null list))

(defmethod empty ((list list))
  nil)

(defmethod to-list ((list list))
  list)

(defmethod for-each ((list list) function)
  (when list
    (funcall function (car list))
    (for-each (cdr list) function)))

(defmethod size ((list list))
  (length list))

(defmethod iterator ((list list))
  (lambda ()
    (cond
      (list
       (let ((head (car list)))
         (setf list (cdr list))
         (values head t)))
      (t
       (values nil nil)))))
