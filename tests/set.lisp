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

(defpackage :pfds.shcl.io/tests/set
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare)
  (:import-from :prove #:is #:ok #:pass #:fail)
  (:export #:test-set))
(in-package :pfds.shcl.io/tests/set)

(defun checked (thing)
  (check-invariants thing)
  thing)

(defun with (set item)
  (checked (with-member set item)))

(defun without (set item)
  (checked (without-member set item)))

(defparameter *sets*
  '(red-black-set
    weight-balanced-set
    unbalanced-set))

(defun compare-< (left right)
  (eq :less (compare left right)))

(defun test-construction ()
  (is (to-list (build))
      nil
      "Empty list produces an empty-set")
  
  (ok (is-empty (build))
      "Empty set is empty")

  (labels
      ((check (input &optional (expected input))
         (let* ((set (build-from-list input))
                (as-list (sort (to-list set) #'compare-<))
                (expected (sort (copy-list expected) #'compare-<)))
           (is as-list expected
               "The set has the expected elements"))))
    (check *sorted-numbers*)
    (check *reverse-sorted-numbers*)
    (check *random-numbers* *random-numbers-uniqued*))

  (values))

(defun test-unnecessary-removal ()
  (let ((set (build-from-list *even-numbers*)))
    (dolist (odd *odd-numbers*)
      (let ((new-set (without set odd)))
        (unless (eq new-set set)
          (fail (format nil "removing ~A resulted in a different set" odd))
          (return-from test-unnecessary-removal)))))
  (pass "Removal of non-members doesn't change the set")
  (values))

(defun test-unnecessary-insert ()
  (let ((set (build-from-list *sorted-numbers*)))
    (dolist (number *sorted-numbers*)
      (let ((new-set (with set number)))
        (unless (eq new-set set)
          (fail (format nil "inserting ~A resulted in a different set" number))
          (return-from test-unnecessary-insert)))))
  (pass "Insert of already-members doesn't change the set")
  (values))

(defun test-unequal-members ()
  (let* ((tokens (loop :for i :below 100 :collect (make-token :value (floor (/ i 5)))))
         (even-numbers (loop :for i :below 50 :collect (* i 2)))
         (set (build-from-list (nconc even-numbers tokens))))
    (ok (not (is-member set (make-token)))
        "Unable to find a fresh token in the set")

    (dolist (token tokens (pass "Tokens were found and removed"))
      (unless (is-member set token)
        (fail "Token couldn't be found")
        (return))
      (let ((new-set (without set token)))
        (when (eq new-set set)
          (fail "Removing a token returned the same set")
          (return))
        (when (is-member new-set token)
          (fail "Token was removed but still present")
          (return))
        (setf set new-set))))
  (values))

(defun test-build-up-and-tear-down ()
  (let* ((set (build))
         (items (concatenate 'list *unequal-objects* *random-numbers*)))
    (dolist (item items)
      (setf set (with set item))
      (unless (is-member set item)
        (fail "A just-inserted item couldn't be found")
        (return-from test-build-up-and-tear-down)))

    (dolist (item items)
      (setf set (without set item))
      (when (is-member set item)
        (fail "A just-removed item couldn't be found")
        (return-from test-build-up-and-tear-down)))

    (ok (is-empty set)
        "The set should be empty after removing everything"))
  (values))

(defun test-set ()
  (named-subtests
    (test-construction)
    (test-unnecessary-removal)
    (test-unnecessary-insert)
    (test-unequal-members)
    (test-build-up-and-tear-down)))
