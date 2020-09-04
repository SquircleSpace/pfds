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

(defpackage :pfds.shcl.io/tests/queue
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:check-invariants #:to-list)
  (:import-from :pfds.shcl.io/tests/common
   #:check-interface-conformance #:do-type-records)
  (:import-from :pfds.shcl.io/interface/queue
   #:is-empty #:empty #:with-last #:without-first #:peek-first
   #:queue)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:import-from :pfds.shcl.io/implementation/batched-queue
   #:make-batched-queue #:batched-queue)
  (:import-from :pfds.shcl.io/implementation/bankers-queue
   #:make-bankers-queue #:bankers-queue)
  (:import-from :prove #:is #:subtest #:ok #:pass)
  (:export #:run-tests))
(in-package :pfds.shcl.io/tests/queue)

(defun with (queue object)
  (let ((result (with-last queue object)))
    (check-invariants result)
    result))

(defun without (queue)
  (multiple-value-bind (result head valid-p) (without-first queue)
    (check-invariants result)
    (if valid-p
        (cassert (not (is-empty queue)) nil "A non-empty queue should always claim validity for without-first returns")
        (cassert (is-empty queue) nil "An empty queue should always claim non-validity for without-first returns"))

    (multiple-value-bind (other-head peek-valid-p) (peek-first queue)
      (cassert (eq other-head head)
               nil
               "Peek-first and without-first should return the same value")
      (cassert (or (and peek-valid-p valid-p)
                   (and (not peek-valid-p) (not valid-p)))
               nil
               "Peek-first and without-first should agree on validity"))

    (values result head valid-p)))

(defun peek (queue)
  (multiple-value-bind (result valid-p) (peek-first queue)
    (if valid-p
        (cassert (not (is-empty queue)) nil "A non-empty queue should always return a true valid-p result for peek")
        (cassert (is-empty queue) nil "An empty queue should always return a nil valid-p result for peek"))
    (values result valid-p)))

(defvar *numbers*
  (loop :for i :below 100 :collect i))

(defvar *big-numbers*
  (loop :for i :in *numbers* :collect (+ 100 i)))

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

(defun constructor (maker)
  (lambda (&optional items)
    (funcall maker :items items)))

(defun test-empty-queues (constructor)
  (let ((q (funcall constructor)))
    (ok (is-empty q)
        "Empty queues are empty")
    (is (peek q)
        nil
        "Peek is expected to return nil on empty queues")
    (is (without q)
        q
        :test #'eq
        "without-first is expected to return the same queue when empty")
    (is (funcall constructor)
        q
        :test #'eq
        "All empty queues are eq")
    (is (without (funcall constructor '(1)))
        q
        :test #'eq
        "Queues that become empty are also eq to other empty queues")))

(defun test-maker (constructor)
  (let* ((numbers *numbers*)
         (q (funcall constructor numbers)))
    (dotimes (i (length numbers))
      (multiple-value-bind (new-q head valid-p) (without q)
        (cassert valid-p nil "Queue shouldn't be empty, yet!")
        (setf q new-q)
        (cassert (eq head (pop numbers)) nil "The queue must return the objects in the same order")))
    (pass "Constructing a queue from a list produces a queue containing the given objects")))

(defun test-mixed-operations (constructor)
  (let ((numbers *numbers*)
        (q (funcall constructor))
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

      (cassert (equal (to-list q)
                      (loop :for i = expected-without-head :then (1+ i)
                            :for count :below length
                            :collect i))
               nil "The queue should contain the right elements"))
    (pass "Messing with the queue produces the right results")))

(defun test-purity (constructor)
  (let* ((q (funcall constructor *numbers*))
         (other-q q))
    (dolist (number *numbers*)
      (setf q (with q number)))
    (setf q (with q 9001))
    (loop :while (not (is-empty q)) :do
      (setf q (without q)))
    (cassert (equal *numbers* (to-list other-q))
             nil "Pointers to old queues remain valid after using with-last and without-first")))

(defun test-queue (maker)
  (subtest (symbol-name maker)
    (let ((constructor (constructor maker)))
      (test-empty-queues constructor)
      (test-maker constructor)
      (test-mixed-operations constructor)
      (test-purity constructor))))

(defparameter *queues*
  '(batched-queue
    bankers-queue))

(defun run-tests (&optional (queues *queues*))
  (do-type-records ((name makers) queues)
    (check-interface-conformance 'queue name)
    (dolist (maker makers)
      (test-queue maker))))
