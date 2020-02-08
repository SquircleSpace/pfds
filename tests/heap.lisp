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
  (:import-from :pfds.shcl.io/utility/compare #:compare)
  (:import-from :pfds.shcl.io/utility/misc #:cassert)
  (:import-from :pfds.shcl.io/interface/common
   #:check-invariants)
  (:import-from :pfds.shcl.io/interface/heap
   #:merge-heaps #:heap-top #:without-heap-top #:with-member #:is-empty #:empty)
  (:import-from :pfds.shcl.io/implementation/leftist-heap #:make-leftist-heap*)
  (:import-from :pfds.shcl.io/implementation/binomial-heap #:make-binomial-heap*)
  (:import-from :pfds.shcl.io/implementation/splay-tree #:make-splay-heap*)
  (:import-from :pfds.shcl.io/implementation/pairing-heap #:make-pairing-heap*)
  (:import-from :prove #:is #:subtest #:ok)
  (:export #:run-tests))
(in-package :pfds.shcl.io/tests/heap)

(defvar *check-invariants* nil)

(defun without (heap)
  (multiple-value-bind (new-heap removed-value success-p) (without-heap-top heap)
    (when *check-invariants*
      (check-invariants new-heap))
    (if (is-empty heap)
        (cassert (not success-p))
        (cassert success-p))
    (multiple-value-bind (peek-top peek-success-p) (heap-top heap)
      (cassert (eq peek-top removed-value))
      (cassert (or (and success-p peek-success-p)
                   (and (not success-p) (not peek-success-p)))))

    (values new-heap removed-value success-p)))

(defun with (heap object)
  (let ((result (with-member heap object)))
    (when *check-invariants*
      (check-invariants result))
    result))

(defvar *sorted-numbers* (loop :for i :below 100000 :collect i))
(defvar *reverse-sorted-numbers* (reverse *sorted-numbers*))
(defvar *random-numbers* (list* 666 666 (loop :for i :below 100000 :collect (random 100000))))
(defvar *random-numbers-sorted* (sort (copy-list *random-numbers*) '<))
(defvar *short-random* (loop :for i :below 1000 :collect (random 1000)))
(defvar *short-random-sorted* (sort (copy-list *short-random*) '<))

(defun make-weight-biased-leftist-heap* (comparator &key items)
  (make-leftist-heap* comparator :bias :weight :items items))

(defun make-height-biased-leftist-heap* (comparator &key items)
  (make-leftist-heap* comparator :bias :height :items items))

(defparameter *makers*
  '(make-weight-biased-leftist-heap*
    make-height-biased-leftist-heap*
    make-binomial-heap*
    make-splay-heap*
    make-pairing-heap*))

(defun heap-sort (heap expected)
  (let (sorted)
    (loop
      (multiple-value-bind (new-heap removed-value success-p) (without heap)
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
          (result-values (multiple-value-list (without empty-heap))))
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

(defun test-with-invariants (constructor)
  (subtest
      "With invariant checking enabled..."
    (let ((*check-invariants* t))
      (subtest
          "Build-up and tear-down of a heap"
        (let ((heap (funcall constructor)))
          (dolist (number *short-random*)
            (setf heap (with heap number)))
          (heap-sort heap *short-random-sorted*)))
      (subtest
          "Constructing a heap"
        (let ((heap (funcall constructor *short-random*)))
          (heap-sort heap *short-random-sorted*))))))

(defun constructor (maker)
  (lambda (&optional items)
    (let ((result (funcall maker 'compare :items items)))
      (when *check-invariants*
        (check-invariants result))
      result)))

(defun test-heap (maker)
  (subtest (symbol-name maker)
    (let ((constructor (constructor maker)))
      (test-construction constructor)
      (test-removal-from-empty constructor)
      (test-with-invariants constructor))))

(defun run-tests ()
  (dolist (maker *makers*)
    (test-heap maker)))
