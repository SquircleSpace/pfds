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

(defpackage :pfds.shcl.io/implementation/pure-list
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:for-each #:size #:iterator)
  (:import-from :pfds.shcl.io/utility/misc
   #:quote-if-symbol)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-iterator-contents)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-adt)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare #:compare* #:compare-objects)
  (:import-from :pfds.shcl.io/interface/list
   #:with-head #:head #:tail #:is-empty #:empty)
  (:import-from :pfds.shcl.io/interface/sequence
   #:with-entry #:lookup-entry #:without-entry
   #:concatenate-sequences #:subsequence
   #:sequence-insert)
  (:export
   #:with-head #:head #:tail #:is-empty #:empty
   #:pure-list #:make-pure-list #:pure-list-p))
(in-package :pfds.shcl.io/implementation/pure-list)

;; See "Purely Functional Data Structures" by Chris Okasaki

(defvar *empty-pure-list*)

(define-adt pure-list
    ()
  (pure-list-nil)
  (pure-list-cons
   head
   (tail *empty-pure-list* :type pure-list)))

(defvar *empty-pure-list*
  (make-pure-list-nil))

(defun make-pure-list (&key items)
  (labels
      ((visit (objects)
         (if objects
             (make-pure-list-cons :head (car objects) :tail (visit (cdr objects)))
             *empty-pure-list*)))
    (visit items)))

(defun pure-list (&rest items)
  (make-pure-list :items items))

(defmethod for-each ((list pure-list-nil) function)
  nil)

(defmethod for-each ((list pure-list-cons) function)
  (loop :until (pure-list-nil-p list) :do
    (progn
      (funcall function (pure-list-cons-head list))
      (setf list (pure-list-cons-tail list)))))

(defun make-pure-list-iterator (list)
  (lambda ()
    (cond
      ((pure-list-nil-p list)
       (values nil nil))

      (t
       (let ((result (pure-list-cons-head list)))
         (setf list (pure-list-cons-tail list))
         (values result t))))))

(defmethod iterator ((list pure-list))
  (make-pure-list-iterator list))

(defmethod with-head ((list pure-list) item)
  (make-pure-list-cons :head item :tail list))

(defmethod head ((list pure-list-cons))
  (values (pure-list-cons-head list) t))

(defmethod head ((list pure-list-nil))
  (values nil nil))

(defmethod tail ((list pure-list-cons))
  (values (pure-list-cons-tail list) (pure-list-cons-head list) t))

(defmethod tail ((list pure-list-nil))
  (values list nil nil))

(defmethod is-empty ((list pure-list-cons))
  nil)

(defmethod is-empty ((list pure-list-nil))
  t)

(defmethod empty ((list pure-list-cons))
  *empty-pure-list*)

(defmethod empty ((list pure-list-nil))
  list)

(defmethod compare-objects ((left pure-list) (right pure-list))
  (compare-iterator-contents (iterator left) (iterator right) #'compare))

(defmethod print-object ((list pure-list) stream)
  (if *print-readably*
      (call-next-method)
      (write `(make-pure-list :items (list ,@(mapcar #'quote-if-symbol (to-list list))))
             :stream stream)))

(defmethod size ((list pure-list-nil))
  0)

(defmethod size ((list pure-list-cons))
  (let ((size 0))
    (loop :until (pure-list-nil-p list) :do
          (progn
            (incf size)
            (setf list (pure-list-cons-tail list))))
    size))

(defun pure-list-with-entry (list index object)
  (let (stack)
    (dotimes (i index)
      (when (pure-list-nil-p list)
        (error "index is out of bounds"))
      (push (pure-list-cons-head list) stack)
      (setf list (pure-list-cons-tail list)))
    (unless (pure-list-nil-p list)
      (setf list (pure-list-cons-tail list)))
    (setf list (make-pure-list-cons :head object :tail list))
    (dolist (value stack)
      (setf list (make-pure-list-cons :head value :tail list)))
    list))

(defmethod with-entry ((list pure-list) index object)
  (check-type index (integer 0))
  (pure-list-with-entry list index object))

(defun pure-list-lookup (list index)
  (dotimes (i index)
    (when (pure-list-nil-p list)
      (return-from pure-list-lookup
        (values nil nil)))
    (setf list (pure-list-cons-tail list)))
  (if (pure-list-nil-p list)
      (values nil nil)
      (values (pure-list-cons-head list) t)))

(defmethod lookup-entry ((list pure-list) index)
  (check-type index (integer 0))
  (pure-list-lookup list index))

(defun pure-list-without (list index)
  (check-type index (integer 0))
  (let (stack)
    (dotimes (i index)
      (when (pure-list-nil-p list)
        (error "index is out of bounds: ~A" index))
      (push (pure-list-cons-head list) stack)
      (setf list (pure-list-cons-tail list)))
    (if (pure-list-nil-p list)
        (error "index is out of bounds: ~A" index)
        (setf list (pure-list-cons-tail list)))
    (dolist (value stack)
      (setf list (make-pure-list-cons :head value :tail list)))
    list))

(defmethod without-entry ((list pure-list) index)
  (pure-list-without list index))

(defmethod concatenate-sequences ((list pure-list) other)
  (when (is-empty other)
    (return-from concatenate-sequences
      list))

  (let ((stack (nreverse (to-list list)))
        (iter (iterator other))
        (result *empty-pure-list*))
    (loop
      (multiple-value-bind (value valid-p) (funcall iter)
        (if valid-p
            (push value stack)
            (return))))
    (dolist (value stack)
      (setf result (make-pure-list-cons :head value :tail result)))
    result))

(defmethod concatenate-sequences ((left pure-list) (right pure-list))
  (when (pure-list-nil-p right)
    (return-from concatenate-sequences
      left))

  (let ((stack (nreverse (to-list left)))
        (result right))
    (dolist (value stack)
      (setf result (make-pure-list-cons :head value :tail result)))
    result))

(defmethod sequence-insert ((list pure-list) before-index object)
  (check-type before-index (integer 0))
  (let (stack)
    (dotimes (i before-index)
      (when (pure-list-nil-p list)
        (error "before-index is out of bounds: ~A" before-index))
      (push (pure-list-cons-head list) stack)
      (setf list (pure-list-cons-tail list)))
    (setf list (make-pure-list-cons :head object :tail list))
    (dolist (value stack)
      (setf list (make-pure-list-cons :head value :tail list)))
    list))

(defmethod subsequence ((list pure-list) min max)
  (check-type min (integer 0))
  (check-type max (or null (integer 0)))
  (when (equal min max)
    (return-from subsequence
      *empty-pure-list*))

  (dotimes (i min)
    (when (pure-list-nil-p list)
      (error "min index is out of bounds: ~A" min))
    (setf list (pure-list-cons-tail list)))

  (unless max
    (return-from subsequence
      list))

  (let (stack
        (result *empty-pure-list*)
        (tail list))
    (dotimes (i (- max min))
      (when (pure-list-nil-p tail)
        (error "max index is out of bounds: ~A" max))
      (push (pure-list-cons-head tail) stack)
      (setf tail (pure-list-cons-tail tail)))

    (when (pure-list-nil-p tail)
      (return-from subsequence
        list))

    (dolist (value stack)
      (setf result (make-pure-list-cons :head value :tail result)))

    result))
