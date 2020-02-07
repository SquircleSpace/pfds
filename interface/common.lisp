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
  (:export
   #:is-empty #:empty #:with-member
   #:to-list
   #:check-invariants
   #:graphviz
   #:print-graphviz
   #:next-graphviz-id
   #:define-interface))
(in-package :pfds.shcl.io/interface/common)

(defgeneric to-list (collection)
  (:documentation
   "Convert the given collection into a list."))

(defgeneric is-empty (collection)
  (:documentation
   "Returns non-nil if the given collection has no elements."))
(defgeneric empty (collection)
  (:documentation
   "Return an empty collection of the same type as the given one."))
(defgeneric with-member (collection item)
  (:documentation
   "Add an object to the given collection."))

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
