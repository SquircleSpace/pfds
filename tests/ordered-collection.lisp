;; Copyright 2020 Ada Avery
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

(defpackage :pfds.shcl.io/tests/ordered-collection
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/utility/misc #:cassert)
  (:import-from :pfds.shcl.io/utility/compare #:compare)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add #:impure-list-builder-extract)
  (:import-from :prove #:is)
  (:export #:test-ordered-collection))
(in-package :pfds.shcl.io/tests/ordered-collection)

(defun list-using-decompose (collection)
  (let ((builder (make-impure-list-builder)))
    (loop :until (is-empty collection) :do
      (multiple-value-bind (new-collection value valid-p) (decompose collection)
        (cassert valid-p)
        (impure-list-builder-add builder value)
        (setf collection new-collection)))
    (impure-list-builder-extract builder)))

(defun compare-equal-ish (l r)
  ;; Let's not be picky about the ordering of unequal objects
  (ecase (compare l r)
    ((:unequal :equal)
     :equal)
    (:less
     :less)
    (:greater
     :greater)))

(defun test-ordered-collection ()
  (is (comparator (build)) *comparator*
      "Empty collections have the right comparator")
  (is (comparator (build 1 2 3)) *comparator*
      "Full collections have the right comparator")

  (is (sort (list-using-decompose (build-from-list *unequal-objects*)) #'compare-less-p)
      (sort (copy-list *unequal-objects*) #'compare-less-p)
      :test #'compare-equal-ish
      "Successive decomposition returns elements in order"))
