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

(defpackage :pfds.shcl.io/implementation/list
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:export))
(in-package :pfds.shcl.io/implementation/list)

(declare-interface-conformance list sequence)

(defun list-push-front (list object)
  (cons object list))

(defun list-pop-front (list)
  (values (cdr list) (car list) (not (null list))))

(defun list-peek-front (list)
  (values (car list) (not (null list))))

(defun list-push-back (list object)
  (append list (list object)))

(defun list-pop-back (list)
  (when (null list)
    (return-from list-pop-back
      (values list nil nil)))

  (let* ((last list)
         (butlast (loop :until (null (cdr last)) :collect (pop last))))
    (values butlast last t)))

(defun list-peek-back (list)
  (let ((last (last list)))
    (values (car last) (not (null last)))))

(defmethod check-invariants ((list list))
  ;; Cool.
  )

(defmethod print-graphviz ((list list) stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (unless list
      (format stream "ID~A [shape=oval, color=gray, label=\"(nil)\"]~%" id)
      (return-from print-graphviz))

    (format stream "ID~A [label=\"~A\"]~%" id (car list))
    (let ((child-id (print-graphviz (cdr list) stream id-vendor)))
      (format stream "ID~A -> ID~A~%" id child-id))

    id))

(defmethod iterator ((list list))
  (lambda ()
    (cond
      (list
       (let ((head (car list)))
         (setf list (cdr list))
         (values head t)))
      (t
       (values nil nil)))))

(defmethod empty ((list list))
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

(defmethod map-members ((list list) function)
  ;; Instead of using mapcar, we'll re-use the map-entries implementation.
  ;; mapcar promises to return a fresh list.  We want to try to share
  ;; data if we can.
  (list-map-entries list (lambda (k v)
                      (declare (ignore k))
                      (funcall function v))))

(defmethod size ((list list))
  (length list))

(defmethod with-member ((list list) object)
  (list-push-front list object))

(defmethod decompose ((list list))
  (list-pop-front list))

(defmethod is-empty ((list list))
  (null list))

(defmethod for-each ((list list) function)
  (dolist (object list)
    (funcall function object)))

(defmethod to-list ((list list))
  list)

(defmethod with-entry ((list list) index value)
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

(defmethod lookup-entry ((list list) index)
  (check-type index (integer 0))
  (dotimes (i index)
    (unless list
      (return-from lookup-entry
        (values nil nil)))
    (setf list (cdr list)))
  (if list
      (values (car list) t)
      (values nil nil)))

(defmethod without-entry ((list list) index)
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

(defmethod for-each-entry ((list list) function)
  (loop :for object :in list :for index :from 0 :do
    (funcall function index object)))

(defmethod map-entries ((list list) function)
  (list-map-entries list function))

;;; Stack

(defmethod with-top ((list list) object)
  (list-push-front list object))

(defmethod without-top ((list list))
  (list-pop-front list))

(defmethod peek-top ((list list))
  (list-peek-front list))

;;; Queue

(defmethod with-back ((list list) object)
  (list-push-back list object))

(defmethod without-front ((list list))
  (list-pop-front list))

(defmethod peek-front ((list list))
  (list-peek-front list))

;;; Deque

(defmethod with-front ((list list) object)
  (list-push-front list object))

(defmethod without-back ((list list))
  (list-pop-back list))

(defmethod peek-back ((list list))
  (list-peek-back list))

;;; Sequence

(defmethod insert ((list list) before-index object)
  (cond
    ((zerop before-index)
     (cons object list))

    (t
     (let ((builder (make-impure-list-builder)))
       (dotimes (i before-index)
         (impure-list-builder-add builder (pop list)))
       (impure-list-builder-add builder object)
       (impure-list-builder-extract builder list)))))

(defmethod join ((left list) (right list))
  (append left right))

(defmethod subsequence ((list list) min max)
  (check-type min (integer 0))
  (check-type max (or null (integer 0)))

  (when (and max (<= max min))
    (return-from subsequence nil))

  (loop :for i :below min :while list :do
    (pop list))

  (when (or (null list) (null max))
    (return-from subsequence list))

  (decf max min)

  (let ((builder (make-impure-list-builder)))
    (dotimes (i max)
      (if list
          (impure-list-builder-add builder (pop list))
          (return)))
    (impure-list-builder-extract builder)))

(defmethod with-first ((list list) object)
  (list-push-front list object))

(defmethod without-first ((list list))
  (list-pop-front list))

(defmethod with-last ((list list) object)
  (list-push-back list object))

(defmethod without-last ((list list))
  (list-pop-back list))
