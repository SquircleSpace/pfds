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
   #:define-interface #:define-adt #:compare #:to-list)
  (:import-from :pfds.shcl.io/heap
   #:merge-heaps #:heap-top #:without-heap-top #:with-member #:is-empty #:empty)
  (:import-from :pfds.shcl.io/impure-queue
   #:make-impure-queue #:enqueue #:dequeue #:impure-queue-count)
  (:export
   #:merge-heaps
   #:heap-top
   #:without-heap-top
   #:with-member
   #:is-empty
   #:empty
   #:make-leftist-heap
   #:make-leftist-heap*
   #:empty-leftist-heap
   #:leftist-heap
   #:height-biased-leftist-heap
   #:weight-biased-leftist-heap))
(in-package :pfds.shcl.io/leftist-heap)

(define-adt guts
    ()
  ((guts-nil (:constructor %make-guts-nil)))
  ((guts-node (:constructor %make-guts-node))
   (rank 0 :type (integer 0))
   (left (guts-nil) :type guts)
   value
   (right (guts-nil) :type guts)))

(defvar *guts-nil*
  (%make-guts-nil))

(defun guts-nil ()
  *guts-nil*)

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

(define-adt leftist-heap
    ((comparator 'compare)
     (guts (guts-nil)))
  ((height-biased-leftist-heap (:constructor %make-height-biased-leftist-heap)))
  ((weight-biased-leftist-heap (:constructor %make-weight-biased-leftist-heap))))

(defun leftist-heap-bias (heap)
  (etypecase heap
    (height-biased-leftist-heap
     :height)
    (weight-biased-leftist-heap
     :weight)))

(defun do-guts-f (guts fn)
  (etypecase guts
    (guts-nil)
    (guts-node
     (do-guts-f (guts-node-left guts) fn)
     (do-guts-f (guts-node-right guts) fn)
     (funcall fn (guts-node-value guts)))))

(defmacro do-guts ((item guts &optional result) &body body)
  `(block nil
     (do-guts-f ,guts (lambda (,item) ,@body))
     ,result))

(defun leftist-heap-to-list (heap)
  (let (items)
    (do-guts (item (leftist-heap-guts heap))
      (push item items))
    items))

(defmethod to-list ((heap height-biased-leftist-heap))
  (leftist-heap-to-list heap))

(defmethod to-list ((heap weight-biased-leftist-heap))
  (leftist-heap-to-list heap))

(defun print-leftist-heap (heap stream)
  (let ((items (leftist-heap-to-list heap)))
    (write
     `(make-leftist-heap* (quote ,(leftist-heap-comparator heap))
                          :bias (quote ,(leftist-heap-bias heap))
                          :items (quote ,items))
     :stream stream)))

(defmethod print-object ((heap height-biased-leftist-heap) stream)
  (print-leftist-heap heap stream))

(defmethod print-object ((heap weight-biased-leftist-heap) stream)
  (print-leftist-heap heap stream))

(defun rank (guts)
  (etypecase guts
    (guts-nil
     0)
    (guts-node
     (guts-node-rank guts))))

(defun make-guts-node (value bias &optional (child1 (guts-nil)) (child2 (guts-nil)))
  (let ((child1-rank (rank child1))
        (child2-rank (rank child2)))
    (when (< child1-rank child2-rank)
      (rotatef child1 child2)
      (rotatef child1-rank child2-rank))
    (%make-guts-node :rank (ecase bias
                             (:height (1+ child2-rank))
                             (:weight (+ 1 child1-rank child2-rank)))
                     :left child1
                     :value value
                     :right child2)))

(defun merge-guts (comparator bias first second)
  (when (guts-nil-p first)
    (return-from merge-guts second))
  (when (guts-nil-p second)
    (return-from merge-guts first))

  (when (eq :greater (funcall comparator (guts-node-value first) (guts-node-value second)))
    (rotatef first second))
  (make-guts-node (guts-node-value first)
                  bias
                  (guts-node-left first)
                  (merge-guts comparator bias (guts-node-right first) second)))

(defun make-guts (comparator bias items)
  (unless items
    (return-from make-guts (guts-nil)))

  ;; We know exactly how large the queue needs to be, and reallocating
  ;; to try and save space as it shrinks is just a waste of time and
  ;; needlessly pressures the GC.
  (let ((queue (make-impure-queue :initial-size (length items)
                                  :shrink-factor nil :growth-factor nil)))
    (dolist (item items)
      (enqueue queue (make-guts-node item bias)))

    (loop :while (< 1 (impure-queue-count queue)) :do
      (let ((first (dequeue queue))
            (second (dequeue queue)))
        (enqueue queue (merge-guts comparator bias first second))))

    (nth-value 0 (dequeue queue))))

(defun make-leftist-heap* (comparator &key (bias :height) items)
  (check-type bias (member :height :weight))
  (let ((guts (make-guts comparator :height items)))
    (ecase bias
      (:height
       (%make-height-biased-leftist-heap :comparator comparator :guts guts))
      (:weight
       (%make-weight-biased-leftist-heap :comparator comparator :guts guts)))))

(defun make-leftist-heap (comparator &rest items)
  (make-leftist-heap* comparator :items items))

(defun empty-leftist-heap (comparator)
  (make-leftist-heap* comparator))

(defun merge-heaps-common (first second constructor)
  (let ((comparator (leftist-heap-comparator first))
        (bias (leftist-heap-bias first)))
    (unless (eq comparator (leftist-heap-comparator second))
      (error "Heaps have different comparators"))
    (assert (eq bias (leftist-heap-bias second)))
    (let* ((first-guts (leftist-heap-guts first))
           (second-guts (leftist-heap-guts second))
           (result-guts (merge-guts comparator bias first-guts second-guts)))
      (when (eql result-guts first-guts)
        (return-from merge-heaps-common first))
      (when (eql result-guts second-guts)
        (return-from merge-heaps-common second))

      (funcall constructor :comparator comparator :guts result-guts))))

(defmethod merge-heaps ((first height-biased-leftist-heap) (second height-biased-leftist-heap))
  (merge-heaps-common first second '%make-height-biased-leftist-heap))

(defmethod merge-heaps ((first weight-biased-leftist-heap) (second weight-biased-leftist-heap))
  (merge-heaps-common first second '%make-weight-biased-leftist-heap))

(defun with-member-common (heap item constructor)
  (let ((comparator (leftist-heap-comparator heap))
        (bias (leftist-heap-bias heap)))
    (funcall constructor
             :comparator comparator
             :guts (merge-guts comparator
                               bias
                               (leftist-heap-guts heap)
                               (make-guts-node item (leftist-heap-bias heap))))))

(defmethod with-member ((heap height-biased-leftist-heap) item)
  (with-member-common heap item '%make-height-biased-leftist-heap))

(defmethod with-member ((heap weight-biased-leftist-heap) item)
  (with-member-common heap item '%make-weight-biased-leftist-heap))

(defun heap-top-common (heap)
  (let ((guts (leftist-heap-guts heap)))
    (if (guts-nil-p guts)
        (values nil nil)
        (values (guts-node-value guts) t))))

(defmethod heap-top ((heap height-biased-leftist-heap))
  (heap-top-common heap))

(defmethod heap-top ((heap weight-biased-leftist-heap))
  (heap-top-common heap))

(defun without-heap-top-common (heap constructor)
  (let ((guts (leftist-heap-guts heap))
        (bias (leftist-heap-bias heap)))
    (when (guts-nil-p guts)
      (return-from without-heap-top-common
        (values heap nil nil)))

    (values
     (funcall constructor
              :comparator (leftist-heap-comparator heap)
              :guts (merge-guts (leftist-heap-comparator heap)
                                bias
                                (guts-node-left guts)
                                (guts-node-right guts)))
     (guts-node-value guts)
     t)))

(defmethod without-heap-top ((heap height-biased-leftist-heap))
  (without-heap-top-common heap '%make-height-biased-leftist-heap))

(defmethod without-heap-top ((heap weight-biased-leftist-heap))
  (without-heap-top-common heap '%make-weight-biased-leftist-heap))

(defmethod empty ((heap height-biased-leftist-heap))
  (%make-height-biased-leftist-heap :comparator (leftist-heap-comparator heap)))

(defmethod empty ((heap weight-biased-leftist-heap))
  (%make-weight-biased-leftist-heap :comparator (leftist-heap-comparator heap)))

(defun is-empty-common (heap)
  (guts-nil-p (leftist-heap-guts heap)))

(defmethod is-empty ((heap height-biased-leftist-heap))
  (is-empty-common heap))

(defmethod is-empty ((heap weight-biased-leftist-heap))
  (is-empty-common heap))
