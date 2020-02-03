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

(defpackage :pfds.shcl.io/tests/heap
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/compare #:compare)
  (:import-from :pfds.shcl.io/heap
   #:merge-heaps #:heap-top #:without-heap-top #:with-member #:is-empty #:empty)
  (:import-from :pfds.shcl.io/leftist-heap #:make-leftist-heap*)
  (:import-from :pfds.shcl.io/binomial-heap #:make-binomial-heap*)
  (:import-from :pfds.shcl.io/splay-tree #:make-splay-heap*)
  (:import-from :pfds.shcl.io/pairing-heap #:make-pairing-heap*)
  (:import-from :prove #:is #:subtest #:ok)
  (:export #:run-tests))
(in-package :pfds.shcl.io/tests/heap)

(defvar *sorted-numbers* (loop :for i :below 100000 :collect i))
(defvar *reverse-sorted-numbers* (reverse *sorted-numbers*))
(defvar *random-numbers* (list* 666 666 (loop :for i :below 100000 :collect (random 100000))))
(defvar *random-numbers-sorted* (sort (copy-list *random-numbers*) '<))

(defun weight-biased-leftist-heap-constructor (&optional items)
  (make-leftist-heap* 'compare :bias :weight :items items))

(defun height-biased-leftist-heap-constructor (&optional items)
  (make-leftist-heap* 'compare :bias :height :items items))

(defun binomial-heap-constructor (&optional items)
  (make-binomial-heap* 'compare :items items))

(defun splay-heap-constructor (&optional items)
  (make-splay-heap* 'compare :items items))

(defun pairing-heap-constructor (&optional items)
  (make-pairing-heap* 'compare :items items))

(defparameter *constructors*
  '(weight-biased-leftist-heap-constructor
    height-biased-leftist-heap-constructor
    binomial-heap-constructor
    splay-heap-constructor
    pairing-heap-constructor))

(defun heap-sort (heap expected)
  (let (sorted)
    (loop
      (multiple-value-bind (new-heap removed-value success-p) (without-heap-top heap)
        (unless success-p
          (return))
        (setf heap new-heap)
        (push removed-value sorted)))
    (setf sorted (nreverse sorted))
    (is sorted expected
        "Heap successfully sorted the provided inputs")
    (values)))

(defun test-construction (constructor)
  (let ((empty-heap (funcall constructor)))
    (ok (is-empty empty-heap)
        "Empty heap is empty"))

  (subtest
   "Heap sort using constructor"
   (heap-sort (funcall constructor *sorted-numbers*) *sorted-numbers*)
   (heap-sort (funcall constructor *reverse-sorted-numbers*) *sorted-numbers*)
   (heap-sort (funcall constructor *random-numbers*) *random-numbers-sorted*))
  (values))

(defun test-removal-from-empty (constructor)
  (subtest
   "Removal of heap-top on empty heap"
   (let* ((empty-heap (funcall constructor))
          (result-values (multiple-value-list (without-heap-top empty-heap))))
     (is (length result-values) 3
         "without-heap-top returns 3 values")
     (destructuring-bind (new-heap removed-value truly-removed-p) result-values
       (is empty-heap new-heap
           :test 'eq
           "Removal from an empty heap returns the same heap")
       (is removed-value nil
           "Removal from an empty heap returns nil value")
       (is truly-removed-p nil
           "Removal from an empty heap returns a nil third value")))))

(defun run-tests ()
  (dolist (constructor *constructors*)
    (subtest
     (symbol-name constructor)
     (test-construction constructor)
     (test-removal-from-empty constructor))))
