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

(uiop:define-package :pfds.shcl.io/tests/stack
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/tests/test-interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/utility/misc #:cassert)
  (:import-from :prove #:is #:ok)
  (:export #:test-stack))
(in-package :pfds.shcl.io/tests/stack)

(defun test-empty ()
  (let* ((e (^make-stack))
         (without-top (multiple-value-list (^without-top e)))
         (peek-top (multiple-value-list (^peek-top e))))
    (is without-top (list e nil nil)
        "WITHOUT-TOP on empty stack produces the expected output")
    (is peek-top (list nil nil)
        "PEEK-TOP on empty produces the expected output")))

(defun test-build-up-and-tear-down ()
  (let ((stack (^make-stack))
        model-stack
        (count 0))
    (loop :for item :in *random-numbers* :do
      (progn
        (let ((new-stack (^with-top stack item)))
          (^check-invariants new-stack)
          (cassert (not (^is-empty new-stack)) nil
                   "The stack shouldn't report that it is empty after adding items")
          (push item model-stack)
          (incf count)
          (setf stack new-stack))))
    (is (^size stack) count
        "Stack has expected size")
    (loop :while model-stack :do
      (progn
        (let ((without-top (multiple-value-list (^without-top stack)))
              (new-model-stack (cdr model-stack))
              (expected-value (car model-stack)))
          (destructuring-bind (new-stack value valid-p) without-top
            (cassert valid-p nil
                     "The stack shouldn't report that it is empty in WITHOUT-TOP return values yet")
            (cassert (equal value expected-value) nil
                     "The stack should return the right objects: got ~A expected ~A"
                     value expected-value)
            (^check-invariants new-stack)
            (setf stack new-stack)
            (setf model-stack new-model-stack)
            (cassert (or (null model-stack) (not (^is-empty new-stack))) nil
                     "The stack shouldn't report that it is empty yet")))))
    (ok (^is-empty stack)
        "Stack should be empty now")
    (is (^size stack) 0
        "Stack should be empty now")))

(defun test-stack ()
  (named-subtests
    (test-empty)
    (test-build-up-and-tear-down)))
