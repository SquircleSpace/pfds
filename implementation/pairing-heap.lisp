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

(defpackage :pfds.shcl.io/implementation/pairing-heap
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-heaps)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-adt #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/impure-queue
   #:make-impure-queue #:enqueue #:dequeue #:impure-queue-count)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:export
   #:pairing-heap
   #:make-pairing-heap
   #:pairing-heap-p))
(in-package :pfds.shcl.io/implementation/pairing-heap)

(define-adt p-heap
    ()
  (p-heap-nil)
  (p-heap-node
   (value (error "value is required"))
   (children nil :type list)))

(defvar *p-heap-nil* (make-p-heap-nil))

(defun p-heap-nil ()
  *p-heap-nil*)

(defun p-heap-find-min (heap)
  (if (p-heap-nil-p heap)
      (values nil nil)
      (values (p-heap-node-value heap) t)))

(defun p-heap-merge (comparator left right)
  (when (p-heap-nil-p left)
    (return-from p-heap-merge right))
  (when (p-heap-nil-p right)
    (return-from p-heap-merge left))

  (if (eq :less (funcall comparator (p-heap-node-value left) (p-heap-node-value right)))
      (copy-p-heap-node left :children (cons right (p-heap-node-children left)))
      (copy-p-heap-node right :children (cons left (p-heap-node-children right)))))

(defun p-heap-insert (comparator heap item)
  (p-heap-merge comparator heap (make-p-heap-node :value item)))

(defun p-heap-merge-pairs (comparator pairs)
  (cond
    ((null pairs)
     (p-heap-nil))
    ((null (cdr pairs))
     (car pairs))
    (t
     (destructuring-bind (first second &rest rest) pairs
       (p-heap-merge comparator
                     (p-heap-merge comparator first second)
                     (p-heap-merge-pairs comparator rest))))))

(defun p-heap-remove-top (comparator heap)
  (if (p-heap-nil-p heap)
      (values heap nil nil)
      (values (p-heap-merge-pairs comparator (p-heap-node-children heap))
              (p-heap-node-value heap)
              t)))

(defmethod print-graphviz ((heap p-heap-nil) stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (format stream "ID~A [shape=box, label=\"(nil)\"]~%" id)
    id))

(defmethod print-graphviz ((heap p-heap-node) stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (format stream "ID~A [label=\"~A\"]~%" id (p-heap-node-value heap))
    (dolist (child (p-heap-node-children heap))
      (let ((child-id (print-graphviz child stream id-vendor)))
        (format stream "ID~A -> ID~A~%" id child-id)))
    id))

(defun make-p-heap (comparator items)
  (unless items
    (return-from make-p-heap
      (values (p-heap-nil) 0)))

  (unless (cdr items)
    (return-from make-p-heap
      (values (make-p-heap-node :value (car items)) 1)))

  ;; We don't *need* to jump through all this rigmarole.  We could
  ;; just find the min, make a list of single-object p-heaps from the
  ;; rest, and make a root node with many children.  That'd be ideal
  ;; if we only wanted to get the minimum element... but who would
  ;; create a heap just to find the min once?  If we use the queue
  ;; technique then we end up with a heap that (hopefully!) has more
  ;; depth to it.  Depth is good.  That's how we keep things cheap.
  (let* ((size (length items))
         (queue (make-impure-queue :initial-size size
                                   :shrink-factor nil
                                   :growth-factor nil)))
    (dolist (item items)
      (enqueue queue (make-p-heap-node :value item)))

    (loop :until (equal 1 (impure-queue-count queue)) :do
          (let ((first (dequeue queue))
                (second (dequeue queue)))
            (enqueue queue (p-heap-merge comparator first second))))
    (values (dequeue queue) size)))

(defun p-heap-node-for-each (heap function)
  (funcall function (p-heap-node-value heap))
  (dolist (child (p-heap-node-children heap))
    (p-heap-node-for-each child function)))

(defun p-heap-for-each (heap function)
  (unless (p-heap-nil-p heap)
    (p-heap-node-for-each heap function)))

(defun make-p-heap-iterator (heap)
  (when (p-heap-nil-p heap)
    (return-from make-p-heap-iterator
      (lambda ()
        (values nil nil))))

  (let ((stack (list (cons heap (p-heap-node-children heap)))))
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
             (return (values (p-heap-node-value tip-node) t)))

            (tip-children
             (let ((child (pop (cdr tip))))
               (push (cons child (p-heap-node-children child)) stack)))

            (t
             (pop stack))))))))

(define-immutable-structure (pairing-heap (:constructor %make-pairing-heap))
  (tree (p-heap-nil) :type p-heap)
  (comparator (error "comparator is required"))
  (size 0 :type (integer 0)))

(declare-interface-conformance pairing-heap priority-queue)

(defun make-pairing-heap (comparator &key items)
  (multiple-value-bind (tree size) (make-p-heap comparator items)
    (%make-pairing-heap :comparator comparator
                        :tree tree
                        :size size)))

(defun pairing-heap (comparator &rest items)
  (make-pairing-heap comparator :items items))

(defmethod meld ((first pairing-heap) (second pairing-heap))
  (unless (eq (pairing-heap-comparator first)
              (pairing-heap-comparator second))
    (error "Attempting to merge heaps with non-eq comparators"))

  (copy-pairing-heap first
                     :tree (p-heap-merge (pairing-heap-comparator first)
                                         (pairing-heap-tree first)
                                         (pairing-heap-tree second))
                     :size (+ (pairing-heap-size first)
                              (pairing-heap-size second))))

(defmethod peek-front ((heap pairing-heap))
  (p-heap-find-min (pairing-heap-tree heap)))

(defun pairing-heap-without-front (heap)
  (when (p-heap-nil-p (pairing-heap-tree heap))
    (return-from pairing-heap-without-front
      (values heap nil nil)))

  (multiple-value-bind
        (new-tree value found-p)
      (p-heap-remove-top (pairing-heap-comparator heap) (pairing-heap-tree heap))
    (values (copy-pairing-heap heap
                               :tree new-tree
                               :size (1- (pairing-heap-size heap)))
            value
            found-p)))

(defmethod without-front ((heap pairing-heap))
  (pairing-heap-without-front heap))

(defmethod decompose ((heap pairing-heap))
  (pairing-heap-without-front heap))

(defmethod with-member ((heap pairing-heap) item)
  (copy-pairing-heap heap
                     :tree (p-heap-insert (pairing-heap-comparator heap)
                                          (pairing-heap-tree heap)
                                          item)
                     :size (1+ (pairing-heap-size heap))))

(defmethod is-empty ((heap pairing-heap))
  (p-heap-nil-p (pairing-heap-tree heap)))

(defmethod empty ((heap pairing-heap))
  (copy-pairing-heap heap :tree (p-heap-nil) :size 0))

(defmethod for-each ((heap pairing-heap) function)
  (p-heap-for-each (pairing-heap-tree heap) function))

(defmethod iterator ((heap pairing-heap))
  (make-p-heap-iterator (pairing-heap-tree heap)))

(defmethod print-object ((heap pairing-heap) stream)
  (if *print-readably*
      (call-next-method)
      (print-container heap stream)))

(defmethod print-graphviz ((heap pairing-heap) stream id-vendor)
  (print-graphviz (pairing-heap-tree heap) stream id-vendor))

(defmethod size ((heap pairing-heap))
  (pairing-heap-size heap))

(defmethod compare-objects ((left pairing-heap) (right pairing-heap))
  (compare-heaps left (pairing-heap-comparator left)
                 right (pairing-heap-comparator right)
                 #'compare))

(defmethod comparator ((heap pairing-heap))
  (pairing-heap-comparator heap))

(defmethod map-members ((heap pairing-heap) function)
  (if (zerop (pairing-heap-size heap))
      heap
      (let (list)
        (for-each heap (lambda (v) (push (funcall function v) list)))
        (make-pairing-heap (pairing-heap-comparator heap) :items list))))

(defun check-order (tree comparator)
  (etypecase tree
    (p-heap-nil)
    (p-heap-node
     (dolist (child (p-heap-node-children tree))
       (cassert (not (eq :greater (funcall comparator
                                           (p-heap-node-value tree)
                                           (p-heap-node-value child))))
                (tree) "The smaller value must be higher in the tree")
       (check-order child comparator)))))

(defmethod check-invariants ((heap pairing-heap))
  (check-order (pairing-heap-tree heap) (pairing-heap-comparator heap)))
