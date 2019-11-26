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

(defpackage :pfds.shcl.io/binomial-heap
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common
   #:define-interface #:define-adt #:define-structure)
  (:import-from :pfds.shcl.io/compare
   #:compare #:define-type-id)
  (:import-from :pfds.shcl.io/heap
   #:merge-heaps #:heap-top #:without-heap-top #:with-member #:is-empty #:empty)
  (:export
   #:merge-heaps
   #:heap-top
   #:without-heap-top
   #:with-member
   #:is-empty
   #:empty
   #:empty-binomial-heap
   #:make-binomial-heap))
(in-package :pfds.shcl.io/binomial-heap)

(define-adt heap-tree
    ()
  (tree-nil)
  (tree-node
   (value)
   (children nil :type list)))

(defvar *tree-nil* (make-tree-nil))

(defun tree-nil ()
  *tree-nil*)

(defun tree-link (left right comparator)
  (when (eq :greater (funcall comparator (tree-node-value left) (tree-node-value right)))
    (rotatef left right))
  (make-tree-node
   :value (tree-node-value left)
   :children (cons right (tree-node-children left))))

(define-structure ranked-tree
  (tree (error "required arg") :type heap-tree)
  (rank (error "required arg") :type (integer 0)))

(defun show-structure (ranked-tree-list &optional (stream *standard-output*))
  (format stream "digraph {~%")
  (let ((id 0))
    (labels
        ((visit (tree)
           (let ((our-id (incf id)))
             (when (tree-nil-p tree)
               (format stream "ID~A [label=\"nil\"]~%" our-id)
               (return-from visit our-id))
             (format stream "ID~A [label=\"~A\"]~%" our-id (tree-node-value tree))
             (dolist (child (tree-node-children tree))
               (let ((child-id (visit child)))
                 (format stream "ID~A -> ID~A~%" our-id child-id)))
             our-id)))

      (dolist (ranked-tree ranked-tree-list)
        (let ((our-id (incf id)))
          (format stream "ID~A [label=\"rank ~A\"]~%" our-id (ranked-tree-rank ranked-tree))
          (let ((tree-id (visit (ranked-tree-tree ranked-tree))))
            (format stream "ID~A -> ID~A~%" our-id tree-id))))))

  (format stream "}~%"))

(defun ranked-tree-link (left right comparator)
  (assert (equal (ranked-tree-rank left)
                 (ranked-tree-rank right)))
  (make-ranked-tree
   :tree (tree-link (ranked-tree-tree left)
                    (ranked-tree-tree right)
                    comparator)
   :rank (1+ (ranked-tree-rank left))))

(defun insert-tree (ranked-tree ranked-tree-list comparator)
  (unless ranked-tree-list
    (return-from insert-tree
      (list ranked-tree)))

  (let ((first-ranked-tree (car ranked-tree-list)))
    (when (< (ranked-tree-rank ranked-tree) (ranked-tree-rank first-ranked-tree))
      (return-from insert-tree (cons ranked-tree ranked-tree-list)))
    (assert (equal (ranked-tree-rank first-ranked-tree)
                   (ranked-tree-rank ranked-tree)))
    (insert-tree
     (ranked-tree-link ranked-tree first-ranked-tree comparator)
     (cdr ranked-tree-list)
     comparator)))

(defun insert-object (object ranked-tree-list comparator)
  (insert-tree
   (make-ranked-tree
    :tree (make-tree-node :value object)
    :rank 0)
   ranked-tree-list
   comparator))

(defun merge-ranked-tree-lists (first second comparator)
  (unless first
    (return-from merge-ranked-tree-lists second))
  (unless second
    (return-from merge-ranked-tree-lists first))

  (let* ((left-head (car first))
         (left-rank (ranked-tree-rank left-head))
         (right-head (car second))
         (right-rank (ranked-tree-rank right-head)))
    (cond
      ((< left-rank right-rank)
       (cons left-head
             (merge-ranked-tree-lists (cdr first) second comparator)))
      ((> left-rank right-rank)
       (cons right-head
             (merge-ranked-tree-lists first (cdr second) comparator)))
      ((= left-rank right-rank)
       (insert-tree
        (ranked-tree-link left-head right-head comparator)
        (merge-ranked-tree-lists (cdr first) (cdr second) comparator)
        comparator)))))

(defun find-minimum (ranked-tree-list comparator)
  (unless ranked-tree-list
    (return-from find-minimum (values nil nil)))

  (let ((min (tree-node-value (ranked-tree-tree (car ranked-tree-list)))))
    (dolist (item (cdr ranked-tree-list))
      (when (eq :less (funcall comparator item min))
        (setf min item)))
    (values min t)))

(defun remove-minimum-tree (ranked-tree-list comparator)
  (assert ranked-tree-list)

  (labels
      ;; This is a bit fancy, but it allows us to find the minimum
      ;; element in the list in a single traversal of the list while
      ;; allocating the minimum number of new cons cells.  A naive
      ;; solution would be to walk the list twice or copy the whole
      ;; list every time.  Is this complexity worth the optimization?
      ;; Probably not!  Was it fun to write?  Definitely!
      ((visit (tree-list current-min-tree current-min-value current-min-index here-index)
         (unless tree-list
           (return-from visit
             (values tree-list current-min-tree current-min-index)))

         (let* ((here-tree (car tree-list))
                (here-value (tree-node-value (ranked-tree-tree here-tree)))
                (comparison (funcall comparator current-min-value here-value)))
           (when (eq :greater comparison)
             (setf current-min-tree here-tree)
             (setf current-min-value here-value)
             (setf current-min-index here-index))

           (multiple-value-bind
                 (new-list min-tree min-index) (visit (cdr tree-list)
                                                      current-min-tree
                                                      current-min-value
                                                      current-min-index
                                                      (1+ here-index))

             (cond
               ;; The dropped element occurs earlier in the list.
               ;; The list we started with is perfectly fine!
               ((< min-index here-index)
                (assert (eq (cdr tree-list) new-list))
                (setf new-list tree-list))

               ;; We're the dropped element!  Nothing to do see here,
               ;; moving on...
               ((= min-index here-index)
                (assert (eq (cdr tree-list) new-list)))

               ;; The element has already been dropped.  Let's attach
               ;; ours to the front of this list.
               ((> min-index here-index)
                (assert (not (eq (cdr tree-list) new-list)))
                (push here-tree new-list)))

             (values
              new-list
              min-tree
              min-index)))))

    (multiple-value-bind
          (new-list min-tree min-index)
        (visit (cdr ranked-tree-list)
               (car ranked-tree-list)
               (tree-node-value (ranked-tree-tree (car ranked-tree-list)))
               0
               1)
      (unless (equal 0 min-index)
        (push (car ranked-tree-list) new-list))
      (values new-list min-tree min-index))))

(defun remove-minimum (ranked-tree-list comparator)
  (multiple-value-bind (updated-ranked-tree-list minimum-tree) (remove-minimum-tree ranked-tree-list comparator)
    (unless minimum-tree
      (return-from remove-minimum
        (values nil nil nil)))

    (let (inner-ranked-tree-list)
      ;; We need to reverse the tree's children and turn them into
      ;; ranked trees
      (loop :for rank :from (ranked-tree-rank minimum-tree) :downto 0
            :for tree :in (tree-node-children (ranked-tree-tree minimum-tree)) :do
              (push (make-ranked-tree :rank rank :tree tree) inner-ranked-tree-list))
      (values
       (merge-ranked-tree-lists updated-ranked-tree-list inner-ranked-tree-list comparator)
       (tree-node-value (ranked-tree-tree minimum-tree))
       t))))

(defun do-tree-f (fn tree)
  (when (tree-nil-p tree)
    (return-from do-tree-f))

  (funcall fn (tree-node-value tree))
  (dolist (child (tree-node-children tree))
    (do-tree-f fn child)))

(defmacro do-tree ((value tree &optional result) &body body)
  `(block nil
     (do-tree-f (lambda (,value) ,@body) ,tree)
     ,result))

(define-structure (binomial-heap (:constructor %make-binomial-heap))
  (comparator 'compare)
  (size 0 :type (integer 0))
  (ranked-trees nil :type list))

(defmethod print-object ((heap binomial-heap) stream)
  (let (items)
    (dolist (ranked-tree (binomial-heap-ranked-trees heap))
      (do-tree (item (ranked-tree-tree ranked-tree))
        (push item items)))
    (write
     `(make-binomial-heap* (quote ,(binomial-heap-comparator heap))
                           :items (quote ,items)))))

(defun make-binomial-heap* (comparator &key items)
  (let (ranked-tree-list
        (count 0))

    ;; Unlike leftist heaps, we can just repeatedly insert the objects
    ;; one by one.  Its O(log(n)) worst case, but O(1) amortized!
    ;; Also, the merging approach seems to be about 3x slower in my
    ;; tests.
    (dolist (item items)
      (incf count)
      (setf ranked-tree-list (insert-object item ranked-tree-list comparator)))

    (%make-binomial-heap
     :comparator comparator
     :size count
     :ranked-trees ranked-tree-list)))

(defun make-binomial-heap (comparator &rest items)
  (make-binomial-heap* comparator :items items))

(defun empty-binomial-heap (comparator)
  (%make-binomial-heap :comparator comparator))

(defun comparator-min (comparator first second)
  (ecase (funcall comparator first second)
    (:greater
     second)
    ((:less :equal :unequal)
     first)))

(defmethod merge-heaps ((first binomial-heap) (second binomial-heap))
  (let ((comparator (binomial-heap-comparator first)))
    (unless (eq comparator (binomial-heap-comparator second))
      (error "Cannot merge heaps with incompatible comparators"))
    (when (zerop (binomial-heap-size first))
      (return-from merge-heaps second))
    (when (zerop (binomial-heap-size second))
      (return-from merge-heaps first))

    (%make-binomial-heap
     :comparator comparator
     :size (+ (binomial-heap-size first)
              (binomial-heap-size second))
     :ranked-trees (merge-ranked-tree-lists
                    (binomial-heap-ranked-trees first)
                    (binomial-heap-ranked-trees second)
                    comparator))))

(defmethod heap-top ((heap binomial-heap))
  (find-minimum (binomial-heap-ranked-trees heap)
                (binomial-heap-comparator heap)))

(defmethod without-heap-top ((heap binomial-heap))
  (when (zerop (binomial-heap-size heap))
    (return-from without-heap-top
      (values heap nil nil)))

  (let ((comparator (binomial-heap-comparator heap)))
    (multiple-value-bind
          (new-ranked-tree-list removed-value success-p)
        (remove-minimum (binomial-heap-ranked-trees heap) comparator)
      (assert success-p nil "Every heap has a minimum")
      (values
       (%make-binomial-heap
        :comparator comparator
        :size (1- (binomial-heap-size heap))
        :ranked-trees new-ranked-tree-list)
       removed-value
       t))))

(defmethod with-member ((heap binomial-heap) item)
  (let ((comparator (binomial-heap-comparator heap)))
    (%make-binomial-heap
     :comparator comparator
     :size (1+ (binomial-heap-size heap))
     :ranked-trees (insert-object item (binomial-heap-ranked-trees heap) comparator))))

(defmethod is-empty ((heap binomial-heap))
  (zerop (binomial-heap-size heap)))

(defmethod empty ((heap binomial-heap))
  (if (zerop (binomial-heap-size heap))
      heap
      (empty-binomial-heap (binomial-heap-comparator heap))))
