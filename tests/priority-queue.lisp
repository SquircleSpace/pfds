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

(defpackage :pfds.shcl.io/tests/priority-queue
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/utility/compare #:compare)
  (:import-from :pfds.shcl.io/utility/misc #:cassert)
  (:import-from :prove #:is)
  (:export #:test-priority-queue))
(in-package :pfds.shcl.io/tests/priority-queue)

(defun without (priority-queue)
  (multiple-value-bind (new-priority-queue removed-value success-p) (without-front priority-queue)
    (check-invariants new-priority-queue)
    (cond
      ((is-empty priority-queue)
       (cassert (not success-p))
       (cassert (equal (size new-priority-queue) 0)))
      (t
       (cassert success-p)
       (cassert (equal (size new-priority-queue) (1- (size priority-queue))))))

    (multiple-value-bind (peek-top peek-success-p) (peek-front priority-queue)
      (cassert (eql peek-top removed-value))
      (cassert (or (and success-p peek-success-p)
                   (and (not success-p) (not peek-success-p)))))

    (values new-priority-queue removed-value success-p)))

(defun with (priority-queue object)
  (let ((result (with-member priority-queue object)))
    (check-invariants result)
    (cassert (equal (size result) (1+ (size priority-queue))))
    result))

(defun compare-ish (l r)
  (ecase (compare l r)
    ((:equal :unequal)
     :equal)
    (:less
     :less)
    (:greater
     :greater)))

(defun priority-queue-sort* (comment priority-queue expected)
  (is (size priority-queue)
      (length expected)
      (format nil "At the outset, priority-queue has the expected size (~A)" comment))
  (let (sorted)
    (loop
      (multiple-value-bind (new-priority-queue removed-value success-p) (without priority-queue)
        (unless success-p
          (return))
        (setf priority-queue new-priority-queue)
        (push removed-value sorted)))
    (setf sorted (nreverse sorted))
    (is sorted expected
        :test #'compare-ish
        (format nil "Priority-Queue successfully sorted the provided inputs (~A)" comment))
    sorted))

(defun priority-queue-sort (comment items)
  (let ((expected (sort (copy-list items) #'compare-less-p))
        (priority-queue (build-from-list items)))
    (priority-queue-sort* comment priority-queue expected)))

(defun test-priority-queue-sort ()
  (priority-queue-sort "sorted" *sorted-numbers*)
  (priority-queue-sort "reverse-sorted" *reverse-sorted-numbers*)
  (priority-queue-sort "unequal" *unequal-objects*))

(defun test-removal-from-empty ()
  (let* ((empty-priority-queue (build))
         (result-values (multiple-value-list (without empty-priority-queue))))
    (is result-values (list empty-priority-queue nil nil)
        "Removal from empty returns the expected values")))

(defun test-build-up-and-tear-down ()
  (let ((priority-queue (build)))
    (dolist (number *unequal-objects*)
      (setf priority-queue (with priority-queue number)))
    (is (size priority-queue)
        (length *unequal-objects*)
        "Size should equal the number of objects added")
    (priority-queue-sort* "unequal" priority-queue (sort (copy-list *unequal-objects*) #'compare-less-p))))

(defun test-priority-queue ()
  (named-subtests
    (test-priority-queue-sort)
    (test-removal-from-empty)
    (test-build-up-and-tear-down)))
