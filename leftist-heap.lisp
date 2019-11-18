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

(defpackage :pfds.shcl.io/leftist-heap
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common
   #:define-interface #:is-empty #:empty #:define-adt #:compare #:with-member)
  (:export
   #:make-leftist-heap
   #:merge-heaps
   #:heap-top
   #:remove-heap-top
   #:with-member
   #:is-empty
   #:empty))
(in-package :pfds.shcl.io/leftist-heap)

(define-interface heap
  (defgeneric merge-heaps (first second))
  (defgeneric heap-top (heap))
  (defgeneric remove-heap-top (heap))
  with-member
  is-empty
  empty)

(define-adt guts
    ()
  ((guts-nil (:constructor %make-guts-nil)))
  ((guts-node (:constructor %make-guts-node))
   (rank 0 :type (integer 0))
   (left (guts-nil) :type guts)
   value
   (right (guts-nil) :type guts)))

(defun show-structure (guts &optional (stream *standard-output*))
  (let ((id 0))
    (labels
        ((visit (guts)
           (when (guts-nil-p guts)
             (return-from visit))
           (let ((our-id (incf id)))
             (format stream "ID~A [label=\"~A (~A)\"]~%" our-id (guts-node-value guts) (guts-node-rank guts))
             (let ((left-id (visit (guts-node-left guts))))
               (when left-id
                 (format stream "ID~A -> ID~A [color=red]~%" our-id left-id)))
             (let ((right-id (visit (guts-node-right guts))))
               (when right-id
                 (format stream "ID~A -> ID~A~%" our-id right-id)))
             our-id)))
      (format stream "digraph {~%")
      (visit guts)
      (format stream "}~%"))))

(defstruct (leftist-heap (:constructor %make-leftist-heap))
  (comparator 'compare :read-only t)
  (guts (%make-guts-nil) :read-only t))

(defun do-guts-f (guts fn)
  (etypecase guts
    (guts-nil)
    (guts-node
     (funcall fn (guts-node-value guts))
     (do-guts-f (guts-node-left guts) fn)
     (do-guts-f (guts-node-right guts) fn))))

(defmacro do-guts ((item guts &optional result) &body body)
  `(block nil
     (do-guts-f ,guts (lambda (,item) ,@body))
     ,result))

(defmethod print-object ((heap leftist-heap) stream)
  (write-char #\( stream)
  (print-object 'make-leftist-heap stream)
  (write-char #\space stream)
  (print-object (leftist-heap-comparator heap) stream)
  (do-guts (item (leftist-heap-guts heap))
    (write-char #\space stream)
    (print-object item stream))
  (write-char #\) stream))

(defvar *guts-nil*
  (%make-guts-nil))

(defun guts-nil ()
  *guts-nil*)

(defun empty-leftist-heap (comparator)
  (%make-leftist-heap :comparator comparator))

(defun rank (guts)
  (etypecase guts
    (guts-nil
     0)
    (guts-node
     (guts-node-rank guts))))

(defun make-guts-node (value &optional
                               (child1 (guts-nil))
                               (child2 (guts-nil)))
  (let ((child1-rank (rank child1))
        (child2-rank (rank child2)))
    (when (< child1-rank child2-rank)
      (rotatef child1 child2)
      (rotatef child1-rank child2-rank))
    (%make-guts-node :rank (1+ child2-rank)
                     :left child1
                     :value value
                     :right child2)))

(defun merge-guts (comparator first second)
  (when (guts-nil-p first)
    (return-from merge-guts second))
  (when (guts-nil-p second)
    (return-from merge-guts first))

  (when (eq :greater (funcall comparator (guts-node-value first) (guts-node-value second)))
    (rotatef first second))
  (make-guts-node (guts-node-value first)
                  (guts-node-left first)
                  (merge-guts comparator (guts-node-right first) second)))

(defun make-guts (comparator items)
  (unless items
    (return-from make-guts (guts-nil)))

  (let* ((items (make-array (length items) :initial-contents items :fill-pointer t))
         (temp (make-array (ceiling (/ (length items) 2)) :fill-pointer 0)))
    (loop :for i :below (length items) :do
      (setf (aref items i) (make-guts-node (aref items i))))

    (loop :while (< 1 (length items)) :do
      (progn
        (loop :for i :below (floor (/ (length items) 2)) :do
          (let ((first (vector-pop items))
                (second (vector-pop items)))
            (vector-push (merge-guts comparator first second) temp)))
        (unless (zerop (length items))
          (assert (equal 1 (length items)))
          (vector-push (vector-pop items) temp))
        (rotatef items temp)))

    (aref items 0)))

(defun make-leftist-heap (comparator &rest items)
  (let ((guts (make-guts comparator items)))
    (%make-leftist-heap :comparator comparator :guts guts)))

(defmethod merge-heaps ((first leftist-heap) (second leftist-heap))
  (let ((comparator (leftist-heap-comparator first)))
    (unless (eq comparator (leftist-heap-comparator second))
      (error "Heaps have different comparators"))
    (merge-guts comparator (leftist-heap-guts first) (leftist-heap-guts second))
    (let* ((first-guts (leftist-heap-guts first))
           (second-guts (leftist-heap-guts second))
           (result-guts (merge-guts comparator first-guts second-guts)))
      (when (eql result-guts first-guts)
        (return-from merge-heaps first))
      (when (eql result-guts second-guts)
        (return-from merge-heaps second))

      (%make-leftist-heap :comparator comparator :guts result-guts))))

(defmethod with-member ((heap leftist-heap) item)
  (let ((comparator (leftist-heap-comparator heap)))
    (%make-leftist-heap :comparator comparator
                        :guts (merge-guts comparator
                                          (leftist-heap-guts heap)
                                          (make-guts-node item)))))

(defmethod heap-top ((heap leftist-heap))
  (let ((guts (leftist-heap-guts heap)))
    (if (guts-nil-p guts)
        (values nil nil)
        (values (guts-node-value guts) t))))

(defmethod remove-heap-top ((heap leftist-heap))
  (let ((guts (leftist-heap-guts heap)))
    (if (guts-nil-p guts)
        heap
        (%make-leftist-heap :comparator (leftist-heap-comparator heap)
                            :guts (merge-guts (leftist-heap-comparator heap)
                                              (guts-node-left guts)
                                              (guts-node-right guts))))))

(defmethod empty ((heap leftist-heap))
  (empty-leftist-heap (leftist-heap-comparator heap)))

(defmethod is-empty ((heap leftist-heap))
  (guts-nil-p (leftist-heap-guts heap)))
