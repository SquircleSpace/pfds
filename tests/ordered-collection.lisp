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

(uiop:define-package :pfds.shcl.io/tests/ordered-collection
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/tests/test-interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/utility/misc #:cassert)
  (:import-from :pfds.shcl.io/utility/compare #:compare)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add #:impure-list-builder-extract)
  (:import-from :prove #:is #:ok)
  (:export #:test-ordered-collection))
(in-package :pfds.shcl.io/tests/ordered-collection)

(defun compare-less-p (l r)
  (eq :less (compare l r)))

(defun compare-equal-ish (l r)
  (let ((result (compare l r)))
    (when (eq :unequal result)
      (setf result :equal))
    result))

(defun list-using-decompose (collection)
  (let ((builder (make-impure-list-builder)))
    (loop :until (^is-empty collection) :do
      (multiple-value-bind (new-collection value valid-p) (^decompose collection)
        (cassert valid-p)
        (impure-list-builder-add builder value)
        (setf collection new-collection)))
    (impure-list-builder-extract builder)))

(defun test-ordered-collection ()
  (subtest* "TEST-BASICS"
    (ok (^comparator (build))
        "Empty collections have a comparator")
    (ok (^comparator (build 1 2 3))
        "Full collections have a comparator")

    (is (list-using-decompose (build-from-list *unequal-objects*))
        (sort (copy-list *unequal-objects*) #'compare-less-p)
        :test #'compare-equal-ish
        "Successive decomposition returns elements in order")))
