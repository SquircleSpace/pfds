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

(uiop:define-package :pfds.shcl.io/implementation/leftist-heap
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/utility/specialization
   #:named-specialize*)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-heaps)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure #:define-adt)
  (:import-from :pfds.shcl.io/utility/impure-queue
   #:make-impure-queue #:enqueue #:dequeue #:impure-queue-count)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:export
   #:<leftist-heap>
   #:<height-biased-leftist-heap>
   #:<weight-biased-leftist-heap>
   #:leftist-heap
   #:leftist-heap-p))
(in-package :pfds.shcl.io/implementation/leftist-heap)

;; See "Purely Functional Data Structures" by Chris Okasaki

(defconstant +default-bias+ :height)

(defvar *guts-nil*)

(defun guts-nil ()
  *guts-nil*)

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

(defun guts-nil-print-graphviz (guts stream id-vendor)
  (declare (ignore guts))
  (let ((id (next-graphviz-id id-vendor)))
    (format stream "ID~A [label=\"nil\"]~%" id)
    id))

(defun guts-node-print-graphviz (guts stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (format stream "ID~A [label=\"~A, ~A\" shape=box]~%" id (guts-node-rank guts) (guts-node-value guts))
    (let ((child-id (guts-print-graphviz (guts-node-left guts) stream id-vendor)))
      (format stream "ID~A -> ID~A~%" id child-id))
    (let ((child-id (guts-print-graphviz (guts-node-right guts) stream id-vendor)))
      (format stream "ID~A -> ID~A~%" id child-id))
    id))

(defun guts-print-graphviz (guts stream id-vendor)
  (if (guts-nil-p guts)
      (guts-nil-print-graphviz guts stream id-vendor)
      (guts-node-print-graphviz guts stream id-vendor)))

(define-immutable-structure (leftist-heap (:constructor %make-leftist-heap))
  (comparator (error "comparator is required"))
  (guts (guts-nil) :type guts)
  (size 0 :type (integer 0))
  (bias +default-bias+ :type (member :height :weight)))

(define-simple-interface-instance <weight-biased-leftist-heap> <<priority-queue>> leftist-heap-
  'make-priority-queue 'make-weight-biased-leftist-heap)

(define-simple-interface-instance <height-biased-leftist-heap> <<priority-queue>> leftist-heap-
  'make-priority-queue 'make-height-biased-leftist-heap)

(define-symbol-macro <leftist-heap> <height-biased-leftist-heap>)

(declaim (inline leftist-heap-print-graphviz))
(defun leftist-heap-print-graphviz (heap stream id-vendor)
  (guts-print-graphviz (leftist-heap-guts heap) stream id-vendor))

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

(declaim (inline leftist-heap-for-each))
(defun leftist-heap-for-each (heap function)
  (do-guts (item (leftist-heap-guts heap))
    (funcall function item)))

(named-specialize*
  (leftist-heap-to-list (collection-to-list <leftist-heap>)))

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

(declaim (inline leftist-heap-iterator))
(defun leftist-heap-iterator (heap)
  (make-tree-iterator (leftist-heap-guts heap)))

(defmethod print-object ((heap leftist-heap) stream)
  (if *print-readably*
      (call-next-method)
      (print-container <leftist-heap> heap stream)))

(declaim (inline rank))
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

(declaim (inline make-leftist-heap))
(defun make-leftist-heap (&key (comparator 'compare) (bias +default-bias+) items)
  (check-type bias (member :height :weight))
  (multiple-value-bind (guts size) (make-guts comparator bias items)
    (%make-leftist-heap :comparator comparator
                        :guts guts
                        :size size
                        :bias bias)))

(declaim (inline make-weight-biased-leftist-heap))
(defun make-weight-biased-leftist-heap (&key (comparator 'compare) items)
  (make-leftist-heap :bias :weight :comparator comparator :items items))

(declaim (inline make-height-biased-leftist-heap))
(defun make-height-biased-leftist-heap (&key (comparator 'compare) items)
  (make-leftist-heap :bias :height :comparator comparator :items items))

(declaim (inline leftist-heap))
(defun leftist-heap (&rest items)
  (make-leftist-heap :items items))

(defun leftist-heap-meld (first second)
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
        (return-from leftist-heap-meld first))
      (when (eql result-guts second-guts)
        (return-from leftist-heap-meld second))

      (%make-leftist-heap :comparator comparator :guts result-guts :size size :bias bias))))

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

(defun leftist-heap-peek-front (heap)
  (let ((guts (leftist-heap-guts heap)))
    (if (guts-nil-p guts)
        (values nil nil)
        (values (guts-node-value guts) t))))

(defun leftist-heap-without-front (heap)
  (let ((guts (leftist-heap-guts heap))
        (bias (leftist-heap-bias heap)))
    (when (guts-nil-p guts)
      (return-from leftist-heap-without-front
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

(declaim (inline leftist-heap-decompose))
(defun leftist-heap-decompose (heap)
  (leftist-heap-without-front heap))

(declaim (inline leftist-heap-empty))
(defun leftist-heap-empty (heap)
  (copy-leftist-heap heap :guts (guts-nil) :size 0))

(declaim (inline leftist-heap-representative-empty))
(defun leftist-heap-representative-empty ()
  (make-leftist-heap))

(declaim (inline leftist-heap-is-empty))
(defun leftist-heap-is-empty (heap)
  (guts-nil-p (leftist-heap-guts heap)))

(declaim (inline leftist-heap-compare))
(defun leftist-heap-compare (left right)
  (compare-heaps <leftist-heap> left right))

(declaim (inline leftist-heap-map-members))
(defun leftist-heap-map-members (heap function)
  (if (zerop (leftist-heap-size heap))
      heap
      (let (list)
        (leftist-heap-for-each heap (lambda (v) (push (funcall function v) list)))
        (make-leftist-heap
         :comparator (leftist-heap-comparator heap)
         :items list
         :bias (leftist-heap-bias heap)))))

(defun check-order (guts comparator)
  (etypecase guts
    (guts-nil)
    (guts-node
     (let ((value (guts-node-value guts)))
       (labels
           ((examine (other)
              (when (guts-node-p other)
                (cassert (not (eq :greater (funcall comparator value (guts-node-value other))))
                         (guts) "The smaller value must always be higher up"))
              (check-order other comparator)))
         (examine (guts-node-left guts))
         (examine (guts-node-right guts)))))))

(defun check-rank (guts bias)
  (etypecase guts
    (guts-nil)
    (guts-node
     (let ((rank (guts-node-rank guts))
           (expected-rank
             (ecase bias
               (:height (1+ (min (rank (guts-node-left guts))
                                 (rank (guts-node-right guts)))))
               (:weight (+ 1 (rank (guts-node-left guts))
                           (rank (guts-node-right guts)))))))
       (cassert (equal rank expected-rank)
                (guts) "Rank must match expected value")
       (cassert (>= (rank (guts-node-left guts))
                    (rank (guts-node-right guts)))
                (guts) "Left rank must not be less than right rank")
       (check-rank (guts-node-left guts) bias)
       (check-rank (guts-node-right guts) bias)))))

(declaim (inline leftist-heap-check-invariants))
(defun leftist-heap-check-invariants (heap)
  (check-order (leftist-heap-guts heap) (leftist-heap-comparator heap))
  (check-rank (leftist-heap-guts heap) (leftist-heap-bias heap)))

(define-interface-methods <leftist-heap> leftist-heap)
