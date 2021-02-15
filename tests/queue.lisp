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

(uiop:define-package :pfds.shcl.io/tests/queue
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/tests/test-interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:import-from :prove #:is #:ok #:pass)
  (:export #:test-queue))
(in-package :pfds.shcl.io/tests/queue)

(defun with (queue object)
  (let ((result (^with-back queue object)))
    (^check-invariants result)
    result))

(defun without (queue)
  (multiple-value-bind (result head valid-p) (^without-front queue)
    (^check-invariants result)
    (if valid-p
        (cassert (not (^is-empty queue)) nil "A non-empty queue should always claim validity for without-first returns")
        (cassert (^is-empty queue) nil "An empty queue should always claim non-validity for without-first returns"))

    (multiple-value-bind (other-head peek-valid-p) (^peek-front queue)
      (cassert (eq other-head head)
               nil
               "Peek-first and without-first should return the same value")
      (cassert (or (and peek-valid-p valid-p)
                   (and (not peek-valid-p) (not valid-p)))
               nil
               "Peek-first and without-first should agree on validity"))

    (values result head valid-p)))

(defun peek (queue)
  (multiple-value-bind (result valid-p) (^peek-front queue)
    (if valid-p
        (cassert (not (^is-empty queue)) nil "A non-empty queue should always return a true valid-p result for peek")
        (cassert (^is-empty queue) nil "An empty queue should always return a nil valid-p result for peek"))
    (values result valid-p)))

(defvar *operations*
  ;; Generated randomly with ~2/3 chance of with, 1/3 chancec of without
  '(with with without without with with with without with with with with without
    with with with with without with with with with with with with without
    with with with without without without without with without with with
    without with with with without with with with without without with with
    with with with with with with with without with without with without
    without with with without without with without without with with without
    with with without without with with without with without without without
    without with without with without without with with without with with
    with without without with without without))

(defun test-empty-queues ()
  (let ((q (^make-queue)))
    (is (multiple-value-list (peek q))
        (list nil nil)
        "PEEK-FRONT is expected to return two nils on empty queues")
    (is (multiple-value-list (without q))
        (list q nil nil)
        :test #'equal
        "WITHOUT-FRONT is expected to return the same queue and two nils when empty")
    (is (^make-queue)
        q
        :test #'eq
        "All empty queues are eq")
    (is (without (^make-queue :items '(1)))
        q
        :test #'eq
        "Queues that become empty are also eq to other empty queues")))

(defun test-maker ()
  (let* ((numbers *sorted-numbers*)
         (q (^make-queue :items numbers)))
    (dotimes (i (length numbers))
      (multiple-value-bind (new-q head valid-p) (without q)
        (cassert valid-p nil "Queue shouldn't be empty, yet!")
        (setf q new-q)
        (cassert (eql head (car numbers)) (head (car numbers)) "The queue must return the objects in the same order.")
        (pop numbers)))
    (pass "Constructing a queue from a list produces a queue containing the given objects")))

(defun test-mixed-operations ()
  (let ((numbers *sorted-numbers*)
        (q (^make-queue))
        (length 0)
        (expected-without-head 0))
    (dolist (operation *operations*)
      (ecase operation
        (with
         (setf q (with q (pop numbers)))
         (incf length))
        (without
         (multiple-value-bind (new-q lost-head valid-p) (without q)
           (cassert valid-p nil "The queue shouldn't be empty!")
           (cassert (equal lost-head expected-without-head) nil "The queue should return objects in the same order they were added")
           (incf expected-without-head)
           (decf length)
           (setf q new-q))))

      (cassert (equal (^to-list q)
                      (loop :for i = expected-without-head :then (1+ i)
                            :for count :below length
                            :collect i))
               nil "The queue should contain the right elements"))
    (pass "Messing with the queue produces the right results")))

(defun test-purity ()
  (let* ((q (^make-queue :items *sorted-numbers*))
         (other-q q))
    (dolist (number *sorted-numbers*)
      (setf q (with q number)))
    (loop :while (not (^is-empty q)) :do
      (setf q (without q)))
    (is (^to-list other-q) *sorted-numbers*
        "Pointers to old queues remain valid after using with-last and without-first")))

(defun test-queue ()
  (named-subtests
    (test-empty-queues)
    (test-maker)
    (test-mixed-operations)
    (test-purity)))
