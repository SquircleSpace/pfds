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

(uiop:define-package :pfds.shcl.io/implementation/lazy-list
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/utility/lazy
   #:force
   #:lazy)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-iterator-contents)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects
   #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-adt)
  (:import-from :pfds.shcl.io/implementation/list
   #:<list>)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder
   #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:export
   #:<lazy-list>
   #:lazy-list
   #:lazy-list-p))
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

(define-simple-interface-instance <lazy-list> <<seq>> lazy-list-
  'make-queue 'make-lazy-list
  'make-deque 'make-lazy-list
  'make-seq 'make-lazy-list
  'make-stack 'make-lazy-list)

(declaim (inline lazy-list-check-invariants))
(defun lazy-list-check-invariants (lazy-list)
  (declare (ignore lazy-list))
  ;; neat!
  )

(defun lazy-list-print-graphviz (lazy-list stream id-vendor)
  (etypecase lazy-list
    (lazy-value
     (let ((id (next-graphviz-id id-vendor)))
       (format stream "ID~A [label=\"(lazy)\", shape=diamond, color=red]~%" id)
       (let ((child-id (lazy-list-print-graphviz (force (lazy-value-suspension lazy-list)) stream id-vendor)))
         (format stream "ID~A -> ID~A~%" id child-id))
       id))

    (lazy-cons
     (let ((id (next-graphviz-id id-vendor)))
       (format stream "ID~A [label=\"~A\", shape=box]~%" id (lazy-cons-head lazy-list))
       (let ((child-id (lazy-list-print-graphviz (lazy-cons-tail lazy-list) stream id-vendor)))
         (format stream "ID~A -> ID~A~%" id child-id))
       id))

    (lazy-nil
     (let ((id (next-graphviz-id id-vendor)))
       (format stream "ID~A [shape=oval, label=\"nil\", color=gray]~%" id)
       id))))

(defvar *empty-lazy-list*
  (make-lazy-nil))

(declaim (inline lazy-list-representative-empty))
(defun lazy-list-representative-empty ()
  *empty-lazy-list*)

(declaim (inline lazy-cons))
(defun lazy-cons (head tail)
  (make-lazy-cons :head head :tail tail))

(declaim (inline lazy-list-with-front))
(defun lazy-list-with-front (list object)
  (lazy-cons object list))

(defun lazy-list-without-front (list)
  (etypecase list
    (lazy-value
     (lazy-list-without-front (force (lazy-value-suspension list))))
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

(defun %lazy-list-join (left right)
  (make-lazy-value
   :suspension
   (lazy
     (multiple-value-bind (new-left value valid-p) (lazy-list-without-front left)
       (if valid-p
           (lazy-cons value (%lazy-list-join new-left right))
           right)))))

(defun lazy-list-join (left right)
  (if (lazy-nil-p right)
      left
      (%lazy-list-join left right)))

(declaim (inline lazy-list-with-back))
(defun lazy-list-with-back (list object)
  (lazy-list-join list (lazy-cons object *empty-lazy-list*)))

(defun lazy-list-without-back (list)
  ;; This isn't anywhere near optimal, but... meh?  What are you doing
  ;; popping from the end of a linked list, anyway?!
  (multiple-value-bind (strict-list value valid-p) (i-without-back <list> (lazy-list-to-list list))
    (values (make-lazy-list :items strict-list) value valid-p)))

(defun lazy-list-peek-back (list)
  (let (value
        valid-p
        (iterator (i-iterator <lazy-list> list)))
    (loop
      (multiple-value-bind (new-value iterator-valid-p) (funcall iterator)
        (unless iterator-valid-p
          (return))
        (setf valid-p t)
        (setf value new-value)))
    (values value valid-p)))

(declaim (inline lazy-list-with-member))
(defun lazy-list-with-member (list object)
  (lazy-cons object list))

(declaim (inline lazy-list-decompose))
(defun lazy-list-decompose (list)
  (lazy-list-without-front list))

(defun lazy-list-lookup-entry (list index)
  (check-type index (integer 0))
  (dotimes (i index)
    (multiple-value-bind (new-list value valid-p) (lazy-list-without-front list)
      (declare (ignore value))
      (unless valid-p
        (return-from lazy-list-lookup-entry
          (values nil nil)))
      (setf list new-list)))
  (lazy-list-peek-front list))

(defun %lazy-list-with-entry (list index object)
  (declare (type (integer 0) index))
  (when (zerop index)
    (return-from %lazy-list-with-entry
      (lazy-cons object (lazy-list-without-front list))))

  (make-lazy-value
   :suspension
   (lazy
     (multiple-value-bind (new-list value valid-p) (lazy-list-without-front list)
       (declare (ignore valid-p))
       (lazy-cons value (%lazy-list-with-entry new-list (1- index) object))))))

(declaim (inline lazy-list-with-entry))
(defun lazy-list-with-entry (list index object)
  (check-type index (integer 0))
  (%lazy-list-with-entry list index object))

(defun %lazy-list-without-entry (list index)
  (declare (type (integer 0) index))
  (cond
    ((zerop index)
     (lazy-list-without-front list))

    ((lazy-nil-p list)
     (values list nil nil))

    (t
     (make-lazy-value
      :suspension
      (lazy
        (multiple-value-bind (new-list value valid-p) (lazy-list-without-front list)
          (if valid-p
              (lazy-cons value (%lazy-list-without-entry new-list (1- index)))
              (values new-list nil nil))))))))

(declaim (inline lazy-list-without-entry))
(defun lazy-list-without-entry (list index)
  (check-type index (integer 0))
  (%lazy-list-without-entry list index))

(declaim (inline lazy-list-append))
(defun lazy-list-append (left right)
  (lazy-list-join left right))

(defun %lazy-list-insert (list before-index object)
  (declare (type (integer 0) before-index))
  (cond
    ((zerop before-index)
     (lazy-cons object list))

    (t
     (make-lazy-value
      :suspension
      (lazy
        (multiple-value-bind (remaining-list value valid-p) (lazy-list-without-front list)
          (declare (ignore valid-p))
          (lazy-cons value (%lazy-list-insert remaining-list (1- before-index) object))))))))

(declaim (inline lazy-list-insert))
(defun lazy-list-insert (list before-index object)
  (check-type before-index (integer 0))
  (%lazy-list-insert list before-index object))

(defun lazy-list-drop-prefix (list count)
  (make-lazy-value
   :suspension
   (lazy
     (loop :for i :below count :do
       (multiple-value-bind (new-list value valid-p) (lazy-list-without-front list)
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
     (multiple-value-bind (new-list value valid-p) (lazy-list-without-front list)
       (if valid-p
           (lazy-cons value (lazy-list-trim new-list (1- max-length)))
           *empty-lazy-list*)))))

(defun lazy-list-subsequence (list min max)
  (check-type min (integer 0))
  (check-type max (or null (integer 0)))
  (when (and max (<= max min))
    (return-from lazy-list-subsequence *empty-lazy-list*))

  (unless (zerop min)
    (setf list (lazy-list-drop-prefix list min)))

  (when max
    (decf max min)
    (setf list (lazy-list-trim list max)))
  list)

(declaim (inline lazy-list-with-first))
(defun lazy-list-with-first (list object)
  (lazy-list-with-front list object))

(declaim (inline lazy-list-without-first))
(defun lazy-list-without-first (list)
  (lazy-list-without-front list))

(declaim (inline lazy-list-peek-first))
(defun lazy-list-peek-first (list)
  (lazy-list-peek-front list))

(declaim (inline lazy-list-with-last))
(defun lazy-list-with-last (list object)
  (lazy-list-with-front list object))

(declaim (inline lazy-list-without-last))
(defun lazy-list-without-last (list)
  (lazy-list-without-front list))

(declaim (inline lazy-list-peek-last))
(defun lazy-list-peek-last (list)
  (lazy-list-peek-front list))

(declaim (inline lazy-list-with-top))
(defun lazy-list-with-top (list object)
  (lazy-list-with-front list object))

(declaim (inline lazy-list-without-top))
(defun lazy-list-without-top (list)
  (lazy-list-without-front list))

(declaim (inline lazy-list-peek-top))
(defun lazy-list-peek-top (list)
  (lazy-list-peek-front list))

(defun lazy-list-is-empty (lazy-list)
  (etypecase lazy-list
    (lazy-cons
     nil)
    (lazy-nil
     t)
    (lazy-value
     (lazy-list-is-empty (force (lazy-value-suspension lazy-list))))))

(declaim (inline lazy-list-empty))
(defun lazy-list-empty (lazy-list)
  (declare (ignore lazy-list))
  *empty-lazy-list*)

(defun lazy-list-for-each-entry (list function)
  (let ((iterator (i-iterator <lazy-list> list))
        (index 0))
    (loop
      (multiple-value-bind (value valid-p) (funcall iterator)
        (unless valid-p
          (return-from lazy-list-for-each-entry nil))
        (funcall function index value)))))

(declaim (inline lazy-list-for-each))
(defun lazy-list-for-each (lazy-list function)
  (lazy-list-for-each-entry lazy-list (lambda (k v)
                                        (declare (ignore k))
                                        (funcall function v))))

(defun %lazy-list-map-entries-lazily (list index function)
  (if (lazy-nil-p list)
      list
      (make-lazy-value
       :suspension
       (lazy
         (multiple-value-bind (new-list value valid-p) (lazy-list-without-front list)
           (if valid-p
               (lazy-cons (funcall function index value)
                          (%lazy-list-map-entries-lazily new-list (1+ index) function))
               *empty-lazy-list*))))))

(declaim (inline lazy-list-map-entries-lazily))
(defun lazy-list-map-entries-lazily (list function)
  (%lazy-list-map-entries-lazily list 0 function))

(defun lazy-list-map-entries-eagerly (list function)
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

(declaim (inline lazy-list-map-entries))
(defun lazy-list-map-entries (lazy-list function)
  (lazy-list-map-entries-lazily lazy-list function))

(declaim (inline lazy-list-map-members))
(defun lazy-list-map-members (lazy-list function)
  (lazy-list-map-entries
   lazy-list
   (lambda (k v)
     (declare (ignore k))
     (funcall function v))))

(defmacro do-lazy-list ((value lazy-list &optional result) &body body)
  (let ((tip (gensym "TIP"))
        (new-tip (gensym "NEW-TIP"))
        (valid-p (gensym "VALID-P"))
        (head (gensym "HEAD")))
    `(loop :with ,tip = ,lazy-list :do
      (multiple-value-bind (,new-tip ,head ,valid-p) (lazy-list-without-front ,tip)
        (unless ,valid-p
          (return ,result))
        (let ((,value ,head))
          ,@body)
        (setf ,tip ,new-tip)))))

(defun lazy-list-to-list (lazy-list)
  (let ((builder (make-impure-list-builder)))
    (do-lazy-list (value lazy-list)
      (impure-list-builder-add builder value))
    (impure-list-builder-extract builder)))

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

(declaim (inline lazy-list))
(defun lazy-list (&rest items)
  (make-lazy-list :items items))

(declaim (inline lazy-list-size))
(defun lazy-list-size (lazy-list)
  (lazy-list-length lazy-list))

(defun lazy-list-iterator (lazy-list)
  (lambda ()
    (multiple-value-bind (new-head result valid-p) (lazy-list-without-front lazy-list)
      (setf lazy-list new-head)
      (values result valid-p))))

(declaim (inline lazy-list-compare))
(defun lazy-list-compare (left right)
  (compare-iterator-contents (lazy-list-iterator left) (lazy-list-iterator right) #'compare))

(declaim (inline lazy-list-as-readable))
(defun lazy-list-as-readable (list)
  ;; Eliminate all lazy thunks from the list so that we only have
  ;; readable elements
  (make-lazy-list :items (lazy-list-to-list list)))

(defmethod print-object ((list lazy-list) stream)
  (if *print-readably*
      (call-next-method (lazy-list-as-readable list) stream)
      (print-container <lazy-list> list stream)))

(define-interface-methods <lazy-list> lazy-list)
