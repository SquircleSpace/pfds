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

(defpackage :pfds.shcl.io/implementation/leftist-heap
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:print-graphviz #:next-graphviz-id #:for-each
   #:size #:iterator)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-heaps)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure #:define-adt)
  (:import-from :pfds.shcl.io/interface/heap
   #:merge-heaps #:heap-top #:without-heap-top #:with-member #:is-empty #:empty)
  (:import-from :pfds.shcl.io/utility/impure-queue
   #:make-impure-queue #:enqueue #:dequeue #:impure-queue-count)
  (:export
   #:merge-heaps
   #:heap-top
   #:without-heap-top
   #:with-member
   #:is-empty
   #:empty
   #:make-leftist-heap
   #:leftist-heap
   #:leftist-heap-p
   #:leftist-heap-comparator))
(in-package :pfds.shcl.io/implementation/leftist-heap)

;; See "Purely Functional Data Structures" by Chris Okasaki

(defconstant +default-bias+ :height)

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

(defmethod print-graphviz ((guts guts-nil) stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (format stream "ID~A [label=\"nil\"]~%" id)
    id))

(defmethod print-graphviz ((guts guts) stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (format stream "ID~A [label=\"~A, ~A\" shape=box]~%" id (guts-node-rank guts) (guts-node-value guts))
    (let ((child-id (print-graphviz (guts-node-left guts) stream id-vendor)))
      (format stream "ID~A -> ID~A~%" id child-id))
    (let ((child-id (print-graphviz (guts-node-right guts) stream id-vendor)))
      (format stream "ID~A -> ID~A~%" id child-id))
    id))

(define-immutable-structure (leftist-heap (:constructor %make-leftist-heap))
  (comparator (error "comparator is required"))
  (guts (guts-nil) :type guts)
  (size 0 :type (integer 0))
  (bias +default-bias+ :type (member :height :weight)))

(defmethod print-graphviz ((heap leftist-heap) stream id-vendor)
  (print-graphviz (leftist-heap-guts heap) stream id-vendor))

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

(defmethod for-each ((heap leftist-heap) function)
  (do-guts (item (leftist-heap-guts heap))
    (funcall function item)))

(defun make-tree-iterator (tree)
  (when (guts-nil-p tree)
    (return-from make-tree-iterator
      (lambda ()
        (values nil nil))))

  (let ((stack (list (list tree (guts-node-left tree) (guts-node-right tree)))))
    (lambda ()
      (loop
        (unless stack
          (return (values nil nil)))

        (let* ((tip (car stack))
               (tip-node (car tip))
               (tip-children (cdr tip)))
          (cond
            (tip-node
             (setf (car tip) nil)
             (return (values (guts-node-value tip-node) t)))

            (tip-children
             (let ((child (pop (cdr tip))))
               (unless (guts-nil-p child)
                 (push (list child (guts-node-left child) (guts-node-right child)) stack))))

            (t
             (pop stack))))))))

(defmethod iterator ((heap leftist-heap))
  (make-tree-iterator (leftist-heap-guts heap)))

(defmethod print-object ((heap leftist-heap) stream)
  (if *print-readably*
      (call-next-method)
      (print-container heap stream)))

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
    (return-from make-guts
      (values (guts-nil) 0)))

  ;; We know exactly how large the queue needs to be, and reallocating
  ;; to try and save space as it shrinks is just a waste of time and
  ;; needlessly pressures the GC.
  (let* ((size (length items))
         (queue (make-impure-queue :initial-size size
                                   :shrink-factor nil :growth-factor nil)))
    (dolist (item items)
      (enqueue queue (make-guts-node item bias)))

    (loop :while (< 1 (impure-queue-count queue)) :do
      (let ((first (dequeue queue))
            (second (dequeue queue)))
        (enqueue queue (merge-guts comparator bias first second))))

    (values (dequeue queue) size)))

(defun make-leftist-heap (comparator &key (bias +default-bias+) items)
  (check-type bias (member :height :weight))
  (multiple-value-bind (guts size) (make-guts comparator bias items)
    (%make-leftist-heap :comparator comparator
                        :guts guts
                        :size size
                        :bias bias)))

(defun leftist-heap (comparator &rest items)
  (make-leftist-heap comparator :items items))

(defun leftist-heap-merge (first second)
  (let ((comparator (leftist-heap-comparator first))
        (bias (leftist-heap-bias first)))
    (unless (eq comparator (leftist-heap-comparator second))
      (error "Heaps have different comparators"))
    (unless (eq bias (leftist-heap-bias second))
      (error "Heaps have different bias"))
    (assert (eq bias (leftist-heap-bias second)))
    (let* ((first-guts (leftist-heap-guts first))
           (second-guts (leftist-heap-guts second))
           (result-guts (merge-guts comparator bias first-guts second-guts))
           (size (+ (leftist-heap-size first)
                    (leftist-heap-size second))))
      (when (eql result-guts first-guts)
        (return-from leftist-heap-merge first))
      (when (eql result-guts second-guts)
        (return-from leftist-heap-merge second))

      (%make-leftist-heap :comparator comparator :guts result-guts :size size :bias bias))))

(defmethod merge-heaps ((first leftist-heap) (second leftist-heap))
  (leftist-heap-merge first second))

(defun leftist-heap-with-member (heap item)
  (let ((comparator (leftist-heap-comparator heap))
        (bias (leftist-heap-bias heap)))
    (%make-leftist-heap
     :comparator comparator
     :guts (merge-guts comparator
                       bias
                       (leftist-heap-guts heap)
                       (make-guts-node item bias))
     :size (1+ (leftist-heap-size heap))
     :bias bias)))

(defmethod with-member ((heap leftist-heap) item)
  (leftist-heap-with-member heap item))

(defun leftist-heap-top (heap)
  (let ((guts (leftist-heap-guts heap)))
    (if (guts-nil-p guts)
        (values nil nil)
        (values (guts-node-value guts) t))))

(defmethod heap-top ((heap leftist-heap))
  (leftist-heap-top heap))

(defun leftist-heap-without-top (heap)
  (let ((guts (leftist-heap-guts heap))
        (bias (leftist-heap-bias heap)))
    (when (guts-nil-p guts)
      (return-from leftist-heap-without-top
        (values heap nil nil)))

    (values
     (%make-leftist-heap
      :comparator (leftist-heap-comparator heap)
      :guts (merge-guts (leftist-heap-comparator heap)
                        bias
                        (guts-node-left guts)
                        (guts-node-right guts))
      :size (1- (leftist-heap-size heap))
      :bias bias)
     (guts-node-value guts)
     t)))

(defmethod without-heap-top ((heap leftist-heap))
  (leftist-heap-without-top heap))

(defmethod empty ((heap leftist-heap))
  (copy-leftist-heap heap :guts (guts-nil) :size 0))

(defun leftist-heap-is-empty (heap)
  (guts-nil-p (leftist-heap-guts heap)))

(defmethod is-empty ((heap leftist-heap))
  (leftist-heap-is-empty heap))

(defmethod size ((heap leftist-heap))
  (leftist-heap-size heap))

(defmethod compare-objects ((left leftist-heap) (right leftist-heap))
  (compare-heaps left (leftist-heap-comparator left)
                 right (leftist-heap-comparator right)
                 #'compare))
