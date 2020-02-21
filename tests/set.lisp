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
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:check-invariants)
  (:import-from :pfds.shcl.io/tests/common
   #:check-common-consistency)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare #:compare-objects)
  (:import-from :pfds.shcl.io/interface/set
   #:is-empty #:empty #:is-member #:with-member #:without-member)
  (:import-from :pfds.shcl.io/implementation/red-black-tree
   #:make-red-black-set #:red-black-set)
  (:import-from :pfds.shcl.io/implementation/unbalanced-tree #:make-unbalanced-set)
  (:import-from :pfds.shcl.io/implementation/weight-balanced-tree
   #:make-weight-balanced-set)
  (:import-from :prove #:is #:subtest #:ok #:pass #:fail)
  (:export #:run-tests #:test-set))
(in-package :pfds.shcl.io/tests/set)

(defun checked (thing)
  (check-invariants thing)
  thing)

(defun with (set item)
  (checked (with-member set item)))

(defun without (set item)
  (checked (without-member set item)))

(defun eql-unique (items)
  (let ((table (make-hash-table :test 'eql))
        collected)
    (loop :for item :in items :do
      (unless (gethash item table)
        (setf (gethash item table) t)
        (push item collected)))
    (nreverse collected)))

(define-immutable-structure token
  value)

(defmethod compare-objects ((left token) (right token))
  (when (eql left right)
    (return-from compare-objects :equal))
  (let ((result (compare (token-value left) (token-value right))))
    (when (eq :equal result)
      (setf result :unequal))
    result))

(defparameter *sorted-numbers* (loop :for i :below 1000 :collect i))
(defparameter *even-numbers* (loop :for i :below 1000 :collect (* 2 i)))
(defparameter *odd-numbers* (mapcar '1+ *even-numbers*))
(defparameter *reverse-sorted-numbers* (reverse *sorted-numbers*))
(defparameter *random-numbers* (list* 666.0 666 666 (loop :for i :below 1000 :collect (random 1000))))
(defparameter *random-numbers-uniqued* (eql-unique *random-numbers*))

(defparameter *makers*
  '(make-red-black-set
    make-weight-balanced-set
    make-unbalanced-set))

(defun compare-< (left right)
  (eq :less (compare left right)))

(defun test-construction (constructor)
  (is (to-list (funcall constructor))
      nil
      "Empty list produces an empty-set")
  
  (ok (is-empty (funcall constructor))
      "Empty set is empty")

  (labels
      ((check (input &optional (expected input))
         (let* ((set (funcall constructor input))
                (as-list (sort (to-list set) #'compare-<))
                (expected (sort (copy-list expected) #'compare-<)))
           (is as-list expected
               "The set has the expected elements"))))
    (check *sorted-numbers*)
    (check *reverse-sorted-numbers*)
    (check *random-numbers* *random-numbers-uniqued*))

  (values))

(defun test-unnecessary-removal (constructor)
  (let ((set (funcall constructor *even-numbers*)))
    (dolist (odd *odd-numbers*)
      (let ((new-set (without set odd)))
        (unless (eq new-set set)
          (fail (format nil "removing ~A resulted in a different set" odd))
          (return-from test-unnecessary-removal)))))
  (pass "Removal of non-members doesn't change the set")
  (values))

(defun test-unnecessary-insert (constructor)
  (let ((set (funcall constructor *sorted-numbers*)))
    (dolist (number *sorted-numbers*)
      (let ((new-set (with set number)))
        (unless (eq new-set set)
          (fail (format nil "inserting ~A resulted in a different set" number))
          (return-from test-unnecessary-insert)))))
  (pass "Insert of already-members doesn't change the set")
  (values))

(defun test-unequal-members (constructor)
  (let* ((tokens (loop :for i :below 100 :collect (make-token :value (floor (/ i 5)))))
         (even-numbers (loop :for i :below 50 :collect (* i 2)))
         (set (funcall constructor (nconc even-numbers tokens))))
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

(defun test-buildup-and-teardown (constructor)
  (let* ((set (funcall constructor))
         (numbers *random-numbers*)
         (tokens (loop :for i :below 100 :collect (make-token :value i)))
         (items (nconc tokens numbers)))
    (dolist (item items)
      (setf set (with set item))
      (unless (is-member set item)
        (fail "A just-inserted item couldn't be found")
        (return-from test-buildup-and-teardown)))

    (dolist (item items)
      (setf set (without set item))
      (when (is-member set item)
        (fail "A just-removed item couldn't be found")
        (return-from test-buildup-and-teardown)))

    (let ((list (to-list set)))
      (is list nil
          "The set should be empty after removing everything"))))

(defun test-basics (constructor)
  (check-common-consistency (funcall constructor))
  (check-common-consistency (funcall constructor *random-numbers*))
  (check-common-consistency (funcall constructor *sorted-numbers*)))

(defun test-set (constructor)
  (test-basics constructor)
  (test-construction constructor)
  (test-unnecessary-removal constructor)
  (test-unnecessary-insert constructor)
  (test-unequal-members constructor)
  (test-buildup-and-teardown constructor))

(defun constructor (maker)
  (lambda (&optional items)
    (funcall maker 'compare :items items)))

(defun run-tests (&optional (makers *makers*))
  (dolist (maker makers)
    (subtest (symbol-name maker)
      (test-set (constructor maker)))))
