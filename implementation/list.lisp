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

(uiop:define-package :pfds.shcl.io/implementation/list
  (:use :common-lisp)
  (:use :pfds.shcl.io/implementation/interface)
  (:use :pfds.shcl.io/utility/interface)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-lists #:compare)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:list-iterator)
  (:export
   #:<list>
   #:list
   #:listp))
(in-package :pfds.shcl.io/implementation/list)

(declaim (inline list-push-front))
(defun list-push-front (list object)
  (cons object list))

(declaim (inline list-pop-front))
(defun list-pop-front (list)
  (values (cdr list) (car list) (not (null list))))

(declaim (inline list-peek-front))
(defun list-peek-front (list)
  (values (car list) (not (null list))))

(declaim (inline list-push-back))
(defun list-push-back (list object)
  (append list (list object)))

(defun list-pop-back (list)
  (when (null list)
    (return-from list-pop-back
      (values list nil nil)))

  (let* ((last list)
         (butlast (loop :until (null (cdr last)) :collect (pop last))))
    (values butlast (car last) t)))

(declaim (inline list-peek-back))
(defun list-peek-back (list)
  (let ((last (last list)))
    (values (car last) (not (null last)))))

(declaim (inline list-check-invariants))
(defun list-check-invariants (list)
  (declare (ignore list))
  ;; Cool.
  )

(defun list-print-graphviz (list stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (unless list
      (format stream "ID~A [shape=oval, color=gray, label=\"(nil)\"]~%" id)
      (return-from list-print-graphviz id))

    (format stream "ID~A [label=\"~A\"]~%" id (car list))
    (let ((child-id (list-print-graphviz (cdr list) stream id-vendor)))
      (format stream "ID~A -> ID~A~%" id child-id))

    id))

(declaim (inline list-representative-empty))
(defun list-representative-empty ()
  nil)

(declaim (inline list-empty))
(defun list-empty (list)
  (declare (ignore list))
  nil)

(defun list-map-entries (list function)
  ;; Because lists are built from the end toward the front, we won't
  ;; really know if we can have data sharing until its too late.  We
  ;; need to record the result of all invocations of function just in
  ;; case something later in the list produces a different result.
  ;; That doesn't mean we can't return a result that shares data!  It
  ;; just means we can't avoid creating ephemeral garbage in the
  ;; process.

  ;; full-stack is a stack of all values we've obtained from function.
  ;; fixed-stack only contains the portion of the list (in reverse
  ;; order) that we'll need to rebuild.  equal-tail represents the
  ;; part of the list that we don't believe needs rebuilding.
  ;; Whenever the function returns something un-eql to the input, we
  ;; adjust fixed-stack and equal-tail accordingly.

  ;; This would be much simpler if written recursively.  We'd recurse
  ;; to get the new tail.  If the tail we get back is unchanged and
  ;; our new head value is unchanged, we return the same list as we
  ;; were given.  I'm writing it this way because I don't want stack
  ;; overflows when dealing with a long list.  Yes, its unlikely that
  ;; we'll have a pure list with thousands of elements... but I'd like
  ;; to support it all the same.
  (let (full-stack
        fixed-stack
        (equal-tail list))
    (loop :for tip = list :then (cdr tip) :while tip
          :for index :from 0 :do
            (progn
              (let* ((old-object (car tip))
                     (new-object (funcall function index old-object)))
                (push new-object full-stack)
                (unless (eql new-object old-object)
                  (setf fixed-stack full-stack)
                  (setf equal-tail (cdr tip))))))
    (let ((result equal-tail))
      (loop :for object :in fixed-stack :do
        (push object result))
      result)))

(declaim (inline list-map-members))
(defun list-map-members (list function)
  ;; Instead of using mapcar, we'll re-use the map-entries implementation.
  ;; mapcar promises to return a fresh list.  We want to try to share
  ;; data if we can.
  (list-map-entries list (lambda (k v)
                           (declare (ignore k))
                           (funcall function v))))

(declaim (inline list-size))
(defun list-size (list)
  (length list))

(declaim (inline list-with-member))
(defun list-with-member (list object)
  (list-push-front list object))

(declaim (inline list-decompose))
(defun list-decompose (list)
  (list-pop-front list))

(declaim (inline list-is-empty))
(defun list-is-empty (list)
  (null list))

(declaim (inline list-for-each))
(defun list-for-each (list function)
  (dolist (object list)
    (funcall function object)))

(declaim (inline list-to-list))
(defun list-to-list (list)
  list)

(defun list-with-entry (list index value)
  (check-type index (integer 0))
  (cond
    ((zerop index)
     (cons value (cdr list)))

    (t
     (let ((builder (make-impure-list-builder)))
       (dotimes (i index)
         (impure-list-builder-add builder (pop list)))
       (impure-list-builder-add builder value)
       (pop list)
       (impure-list-builder-extract list)))))

(defun list-lookup-entry (list index)
  (check-type index (integer 0))
  (dotimes (i index)
    (unless list
      (return-from list-lookup-entry
        (values nil nil)))
    (setf list (cdr list)))
  (if list
      (values (car list) t)
      (values nil nil)))

(defun list-without-entry (list index)
  (check-type index (integer 0))
  (cond
    ((null list)
     (values list nil nil))

    ((zerop index)
     (values (cdr list) (car list) t))

    (t
     (let ((builder (make-impure-list-builder))
           removed-value)
       (dotimes (i index)
         (unless list
           (error "index out of bounds: ~A" index))
         (impure-list-builder-add builder (pop list)))
       (setf removed-value (pop list))
       (values (impure-list-builder-extract list) removed-value t)))))

(declaim (inline list-for-each-entry))
(defun list-for-each-entry (list function)
  (loop :for object :in list :for index :from 0 :do
    (funcall function index object)))

;;; Stack

(declaim (inline list-with-top))
(defun list-with-top (list object)
  (list-push-front list object))

(declaim (inline list-without-top))
(defun list-without-top (list)
  (list-pop-front list))

(declaim (inline list-peek-top))
(defun list-peek-top (list)
  (list-peek-front list))

(declaim (inline list-reverse))
(defun list-reverse (list)
  (reverse list))

;;; Queue

(declaim (inline list-with-back))
(defun list-with-back (list object)
  (list-push-back list object))

(declaim (inline list-without-front))
(defun list-without-front (list)
  (list-pop-front list))

;;; Deque

(declaim (inline list-with-front))
(defun list-with-front (list object)
  (list-push-front list object))

(declaim (inline list-without-back))
(defun list-without-back (list)
  (list-pop-back list))

;;; Sequence

(defun list-insert (list before-index object)
  (cond
    ((zerop before-index)
     (cons object list))

    (t
     (let ((builder (make-impure-list-builder)))
       (dotimes (i before-index)
         (impure-list-builder-add builder (pop list)))
       (impure-list-builder-add builder object)
       (impure-list-builder-extract builder list)))))

(declaim (inline list-join))
(defun list-join (left right)
  (append left right))

(defun list-subsequence (list min max)
  (check-type min (integer 0))
  (check-type max (or null (integer 0)))

  (when (and max (<= max min))
    (return-from list-subsequence nil))

  (loop :for i :below min :while list :do
    (pop list))

  (when (or (null list) (null max))
    (return-from list-subsequence list))

  (decf max min)

  (let ((builder (make-impure-list-builder)))
    (dotimes (i max)
      (if list
          (impure-list-builder-add builder (pop list))
          (return)))
    (impure-list-builder-extract builder)))

(declaim (inline list-with-first))
(defun list-with-first (list object)
  (list-push-front list object))

(declaim (inline list-without-first))
(defun list-without-first (list)
  (list-pop-front list))

(declaim (inline list-with-last))
(defun list-with-last (list object)
  (list-push-back list object))

(declaim (inline list-without-last))
(defun list-without-last (list)
  (list-pop-back list))

(declaim (inline list-make-seq))
(defun list-make-sequence (&key items)
  items)

(declaim (inline list-make-stack))
(defun list-make-stack (&key items)
  items)

(declaim (inline list-make-queue))
(defun list-make-queue (&key items)
  items)

(declaim (inline list-make-deque))
(defun list-make-deque (&key items)
  items)

(define-simple-interface-instance <list> <<seq>> list-
  'compare 'compare-lists)

(define-interface-methods <list> list)
