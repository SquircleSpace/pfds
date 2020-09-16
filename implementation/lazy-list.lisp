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

(defpackage :pfds.shcl.io/implementation/lazy-list
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/lazy #:force #:lazy)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-iterator-contents)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-adt)
  (:export
   #:make-lazy-list
   #:lazy-list
   #:lazy-list-p
   #:lazy-list-append
   #:lazy-list-reverse
   #:lazy-list-length
   #:lazy-list-map-entries-lazily))
(in-package :pfds.shcl.io/implementation/lazy-list)

;; See "Purely Functional Data Structures" by Chris Okasaki.

(defvar *empty-lazy-list*)

(define-adt lazy-list
    ()
  (lazy-nil)
  (lazy-cons
   head
   (tail *empty-lazy-list*))
  (lazy-value
   (suspension (error "suspension is required"))))

(declare-interface-conformance lazy-list sequence)

(defmethod check-invariants ((lazy-list lazy-list))
  ;; neat!
  )

(defmethod print-graphviz ((lazy-nil lazy-nil) stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (format stream "ID~A [shape=oval, label=\"nil\", color=gray]~%" id)
    id))

(defmethod print-graphviz ((lazy-cons lazy-cons) stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (format stream "ID~A [label=\"~A\", shape=box]~%" id (lazy-cons-head lazy-cons))
    (let ((child-id (print-graphviz (lazy-cons-tail lazy-cons) stream id-vendor)))
      (format stream "ID~A -> ID~A~%" id child-id))
    id))

(defmethod print-graphviz ((lazy-value lazy-value) stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (format stream "ID~A [label=\"(lazy)\", shape=diamond, color=red]~%" id)
    (let ((child-id (print-graphviz (force (lazy-value-suspension lazy-value)) stream id-vendor)))
      (format stream "ID~A -> ID~A~%" id child-id))
    id))

(defvar *empty-lazy-list*
  (make-lazy-nil))

(defun lazy-cons (head tail)
  (make-lazy-cons :head head :tail tail))

(defun lazy-list-push-front (list object)
  (lazy-cons object list))

(defun lazy-list-pop-front (list)
  (etypecase list
    (lazy-value
     (lazy-list-pop-front (force (lazy-value-suspension list))))
    (lazy-nil
     (values list nil nil))
    (lazy-cons
     (values (lazy-cons-tail list) (lazy-cons-head list) t))))

(defun lazy-list-peek-front (list)
  (etypecase list
    (lazy-value
     (lazy-list-peek-front (force (lazy-value-suspension list))))
    (lazy-nil
     (values nil nil))
    (lazy-cons
     (values (lazy-cons-head list) t))))

(defun lazy-list-empty-p (list)
  (not (nth-value 2 (lazy-list-peek-front list))))

(defun %lazy-list-append (left right)
  (make-lazy-value
   :suspension
   (lazy
     (multiple-value-bind (new-left value valid-p) (lazy-list-pop-front left)
       (if valid-p
           (lazy-cons value (%lazy-list-append new-left right))
           right)))))

(defun lazy-list-append (left right)
  (if (lazy-nil-p right)
      left
      (%lazy-list-append left right)))

(defun lazy-list-push-back (list object)
  (lazy-list-append list (lazy-cons object *empty-lazy-list*)))

(defun lazy-list-pop-back (list)
  ;; This isn't anywhere near optimal, but... meh?  What are you doing
  ;; popping from the end of a linked list, anyway?!
  (multiple-value-bind (strict-list value valid-p) (without-last (to-list list))
    (values (make-lazy-list :items strict-list) value valid-p)))

(defun lazy-list-peek-back (list)
  (let (value
        valid-p
        (iterator (iterator list)))
    (loop
      (multiple-value-bind (new-value iterator-valid-p) (funcall iterator)
        (unless iterator-valid-p
          (return))
        (setf valid-p t)
        (setf value new-value)))
    (values value valid-p)))

(defmethod with-member ((list lazy-list) object)
  (lazy-cons object list))

(defmethod decompose ((list lazy-list))
  (lazy-list-pop-front list))

(defmethod with-top ((list lazy-list) object)
  (lazy-list-push-front list object))

(defmethod without-top ((list lazy-list))
  (lazy-list-pop-front list))

(defmethod peek-top ((list lazy-list))
  (lazy-list-peek-front list))

(defmethod lookup-entry ((list lazy-list) index)
  (check-type index (integer 0))
  (dotimes (i index)
    (multiple-value-bind (new-list value valid-p) (lazy-list-pop-front list)
      (declare (ignore value))
      (unless valid-p
        (return-from lookup-entry
          (values nil nil)))
      (setf list new-list)))
  (lazy-list-peek-front list))

(defun lazy-list-with-entry (list index object)
  (when (zerop index)
    (return-from lazy-list-with-entry
      (lazy-cons object (lazy-list-pop-front list))))

  (make-lazy-value
   :suspension
   (lazy
     (multiple-value-bind (new-list value valid-p) (lazy-list-pop-front list)
       (declare (ignore valid-p))
       (lazy-cons value (lazy-list-with-entry new-list (1- index) object))))))

(defmethod with-entry ((list lazy-list) index object)
  (check-type index (integer 0))
  (lazy-list-with-entry list index object))

(defun lazy-list-without-entry (list index)
  (cond
    ((zerop index)
     (lazy-list-pop-front list))

    ((lazy-nil-p list)
     (values list nil nil))

    (t
     (make-lazy-value
      :suspension
      (lazy
        (multiple-value-bind (new-list value valid-p) (lazy-list-pop-front list)
          (if valid-p
              (lazy-cons value (lazy-list-without-entry new-list (1- index)))
              (values new-list nil nil))))))))

(defmethod without-entry ((list lazy-list) index)
  (check-type index (integer 0))
  (lazy-list-without-entry list index))

(defmethod join ((left lazy-list) (right lazy-list))
  (lazy-list-append left right))

(defun lazy-list-insert (list before-index object)
  (cond
    ((zerop before-index)
     (lazy-cons object list))

    (t
     (make-lazy-value
      :suspension
      (lazy
        (multiple-value-bind (remaining-list value valid-p) (lazy-list-pop-front list)
          (declare (ignore valid-p))
          (lazy-cons value (lazy-list-insert remaining-list (1- before-index) object))))))))

(defmethod insert ((list lazy-list) before-index object)
  (check-type before-index (integer 0))
  (lazy-list-insert list before-index object))

(defun lazy-list-drop-prefix (list count)
  (make-lazy-value
   :suspension
   (lazy
     (loop :for i :below count :do
       (multiple-value-bind (new-list value valid-p) (lazy-list-pop-front list)
         (declare (ignore value))
         (setf list new-list)
         (unless valid-p
           (return))))

     list)))

(defun lazy-list-trim (list max-length)
  (when (zerop max-length)
    (return-from lazy-list-trim *empty-lazy-list*))

  (make-lazy-value
   :suspension
   (lazy
     (multiple-value-bind (new-list value valid-p) (lazy-list-pop-front list)
       (if valid-p
           (lazy-cons value (lazy-list-trim new-list (1- max-length)))
           *empty-lazy-list*)))))

(defun lazy-list-subsequence (list min max)
  (when (and max (<= max min))
    (return-from lazy-list-subsequence *empty-lazy-list*))

  (unless (zerop min)
    (setf list (lazy-list-drop-prefix list min)))

  (when max
    (decf max min)
    (setf list (lazy-list-trim list max)))
  list)

(defmethod subsequence ((list lazy-list) min max)
  (check-type min (integer 0))
  (check-type max (or null (integer 0)))
  (lazy-list-subsequence list min max))

(defmethod with-first ((list lazy-list) object)
  (lazy-list-push-front list object))

(defmethod without-first ((list lazy-list))
  (lazy-list-pop-front list))

(defmethod peek-first ((list lazy-list))
  (lazy-list-peek-front list))

(defmethod with-last ((list lazy-list) object)
  (lazy-list-push-front list object))

(defmethod without-last ((list lazy-list))
  (lazy-list-pop-front list))

(defmethod peek-last ((list lazy-list))
  (lazy-list-peek-front list))

(defmethod with-top ((list lazy-list) object)
  (lazy-list-push-front list object))

(defmethod without-top ((list lazy-list))
  (lazy-list-pop-front list))

(defmethod peek-top ((list lazy-list))
  (lazy-list-peek-front list))

(defmethod with-front ((list lazy-list) object)
  (lazy-list-push-front list object))

(defmethod without-front ((list lazy-list))
  (lazy-list-pop-front list))

(defmethod peek-front ((list lazy-list))
  (lazy-list-peek-front list))

(defmethod with-back ((list lazy-list) object)
  (lazy-list-push-back list object))

(defmethod without-back ((list lazy-list))
  (lazy-list-pop-back list))

(defmethod peek-back ((list lazy-list))
  (lazy-list-peek-back list))

(defmethod is-empty ((lazy-cons lazy-cons))
  nil)

(defmethod is-empty ((lazy-nil lazy-nil))
  t)

(defmethod is-empty ((lazy-value lazy-value))
  (is-empty (force (lazy-value-suspension lazy-value))))

(defmethod empty ((lazy-list lazy-list))
  *empty-lazy-list*)

(defun lazy-list-for-each-entry (list function)
  (let ((iterator (iterator list))
        (index 0))
    (loop
      (multiple-value-bind (value valid-p) (funcall iterator)
        (unless valid-p
          (return-from lazy-list-for-each-entry nil))
        (funcall function index value)))))

(defmethod for-each-entry ((lazy-list lazy-list) function)
  (lazy-list-for-each-entry lazy-list function))

(defmethod for-each ((lazy-list lazy-list) function)
  (lazy-list-for-each-entry lazy-list (lambda (k v)
                                        (declare (ignore k))
                                        (funcall function v))))

(defun %lazy-list-map-entries-lazily (list index function)
  (if (lazy-nil-p list)
      list
      (make-lazy-value
       :suspension
       (lazy
         (multiple-value-bind (new-list value valid-p) (lazy-list-pop-front list)
           (if valid-p
               (lazy-cons (funcall function index value)
                          (%lazy-list-map-entries-lazily new-list (1+ index) function))
               *empty-lazy-list*))))))

(defun lazy-list-map-entries-lazily (list function)
  (%lazy-list-map-entries-lazily list 0 function))

(defun lazy-list-map-entries (list function)
  (let (stack
        (index 0))
    (loop
      (etypecase list
        (lazy-value
         (setf list (force (lazy-value-suspension list))))
        (lazy-cons
         (push (funcall function index (lazy-cons-head list)) stack)
         (setf list (lazy-cons-tail list))
         (incf index))
        (lazy-nil
         (return))))
    (let ((result *empty-lazy-list*))
      (dolist (item stack)
        (setf result (lazy-cons item result)))
      result)))

(defmethod map-entries ((lazy-list lazy-list) function)
  (lazy-list-map-entries lazy-list function))

(defmethod map-members ((lazy-list lazy-list) function)
  (lazy-list-map-entries lazy-list (lambda (k v)
                                        (declare (ignore k))
                                        (funcall function v))))

(defmacro do-lazy-list ((value lazy-list &optional result) &body body)
  (let ((tip (gensym "TIP"))
        (new-tip (gensym "NEW-TIP"))
        (valid-p (gensym "VALID-P"))
        (head (gensym "HEAD")))
    `(loop :with ,tip = ,lazy-list :do
      (multiple-value-bind (,new-tip ,head ,valid-p) (lazy-list-pop-front ,tip)
        (unless ,valid-p
          (return ,result))
        (let ((,value ,head))
          ,@body)
        (setf ,tip ,new-tip)))))

(defun lazy-list-reverse (lazy-list)
  (make-lazy-value
   :suspension
   (lazy
     (let ((result *empty-lazy-list*))
       (do-lazy-list (value lazy-list)
         (setf result (lazy-cons value result)))
       result))))

(defun lazy-list-length (lazy-list)
  (let ((count 0))
    (do-lazy-list (value lazy-list)
      (declare (ignore value))
      (incf count))
    count))

(defun make-lazy-list (&key items)
  (let ((item-stack (reverse items))
        (result-stack *empty-lazy-list*))
    (dolist (item item-stack)
      (setf result-stack (lazy-cons item result-stack)))
    result-stack))

(defun lazy-list (&rest items)
  (make-lazy-list :items items))

(defmethod size ((lazy-list lazy-list))
  (lazy-list-length lazy-list))

(defmethod iterator ((lazy-list lazy-list))
  (lambda ()
    (multiple-value-bind (new-head result valid-p) (lazy-list-pop-front lazy-list)
      (setf lazy-list new-head)
      (values result valid-p))))

(defmethod compare-objects ((left lazy-list) (right lazy-list))
  (compare-iterator-contents (iterator left) (iterator right) #'compare))
