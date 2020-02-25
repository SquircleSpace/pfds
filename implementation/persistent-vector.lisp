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

(defpackage :pfds.shcl.io/implementation/persistent-vector
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:check-invariants #:for-each #:for-each-kv
   #:with-entry #:lookup-entry #:size #:iterator
   #:without-entry)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-containers)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-sequence)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/interface/list
   #:with-head #:head #:tail #:is-empty #:empty)
  (:import-from :pfds.shcl.io/interface/sequence
   #:sequence-insert #:concatenate-sequences #:subsequence)
  (:export
   #:with-head
   #:head
   #:tail
   #:with-entry
   #:without-entry
   #:lookup-entry
   #:sequence-insert
   #:concatenate-sequences
   #:subsequence
   #:is-empty
   #:empty
   #:make-persistent-vector
   #:persistent-vector
   #:persistent-vector-p))
(in-package :pfds.shcl.io/implementation/persistent-vector)

;; See Clojure's PersistentVector
;; Inspired by
;; https://github.com/clojure/clojure/blob/e5fc803ff13f783661ef9491842719ab68a245ca/src/jvm/clojure/lang/PersistentVector.java

(defconstant +branching-factor+ 32)

(defparameter *empty-array* (make-array +branching-factor+ :initial-element nil :fill-pointer 0))

(defun make-vector (&optional (prototype *empty-array*))
  (let ((result (make-array +branching-factor+ :initial-element nil :fill-pointer 0)))
    (dotimes (i (length prototype))
      (vector-push (aref prototype i) result))
    result))

(defun vector-copy-and-push (vector object)
  (let ((result (make-vector vector)))
    (vector-push object result)
    result))

(defun vector-copy-and-pop (vector)
  (let ((result (make-vector vector)))
    (setf (aref result (1- (length result))) nil)
    (vector-pop result)
    result))

(defun vector-full-p (vector)
  (equal +branching-factor+ (length vector)))

(defun vector-with-update (vec value index)
  (when (eql (aref vec index) value)
    (return-from vector-with-update vec))

  (let ((result (make-vector vec)))
    (setf (aref result index) value)
    result))

(defun make-singular-vec-tree (object height)
  (vector-copy-and-push *empty-array* (if (equal height 1)
                                          object
                                          (make-singular-vec-tree object (1- height)))))

(define-immutable-structure (persistent-vector (:constructor %make-persistent-vector))
  ;; Height up to 32767 is allowed.  Really, that's much taller than
  ;; anyone will ever need.  Even with a branching factor of 2, that
  ;; permits a tree with 7*10^9863 elements.  For comparison, the
  ;; estimated number of particles in the observable universe is about
  ;; 10^80.  Unless we discover we are very wrong about physics, the
  ;; universe cannot contain a tree that tall.  I think I can safely
  ;; say 10^9863 "ought to be enough for anyone".
  (height 1 :type (and fixnum (integer 0)))
  (count 0 :type (integer 0))
  (tree *empty-array* :type vector))

(defvar *empty-persistent-vector* (%make-persistent-vector))

(defun vector-tree-push (vec height object)
  (labels
      ((visit (vec height object)
         (cond
           ((equal height 1)
            (if (vector-full-p vec)
                vec ;; Can't fit it here!
                (vector-copy-and-push vec object)))

           (t
            (let* ((last-element (aref vec (1- (length vec))))
                   (updated-last (visit last-element (1- height) object)))
              (if (eq updated-last last-element)
                  (if (vector-full-p vec)
                      vec
                      (vector-copy-and-push vec (make-singular-vec-tree object (1- height))))
                  (vector-with-update vec updated-last (1- (length vec)))))))))

    (let ((result (visit vec height object)))
      (if (eq result vec)
          (if (vector-full-p vec)
              (let ((new-result (make-vector)))
                (vector-push result new-result)
                (vector-push (make-singular-vec-tree object height) new-result)
                (values new-result (1+ height)))
              (values (vector-copy-and-push result (make-singular-vec-tree object (1- height))) height))
          (values result height)))))

(defun persistent-vector-push (p-vec object)
  (multiple-value-bind
        (new-tree new-height)
      (vector-tree-push (persistent-vector-tree p-vec) (persistent-vector-height p-vec) object)
    (copy-persistent-vector p-vec :tree new-tree :height new-height :count (1+ (persistent-vector-count p-vec)))))

(defun vector-tree-pop (vec height)
  (labels
      ((visit (vec height)
         (cond
           ((equal height 1)
            (values
             (if (equal 1 (length vec))
                 nil
                 (vector-copy-and-pop vec))
             (aref vec (1- (length vec)))))

           (t
            (multiple-value-bind (updated-last value) (visit (aref vec (1- (length vec))) (1- height))
              (values
               (if updated-last
                   (vector-with-update vec updated-last (1- (length vec)))
                   (if (equal 1 (length vec))
                       nil
                       (vector-copy-and-pop vec)))
               value))))))

    (multiple-value-bind (result-tree popped-value) (visit vec height)
      (cond
        ((null result-tree)
         (values *empty-array* popped-value 1))
        ((and (equal 1 (length result-tree))
              (> height 1))
         (values (aref result-tree 0) popped-value (1- height)))
        (t
         (values result-tree popped-value height))))))

(defun persistent-vector-pop (p-vec)
  (when (zerop (persistent-vector-count p-vec))
    (return-from persistent-vector-pop
      (values nil nil nil)))

  (multiple-value-bind
        (new-tree popped-value new-height)
      (vector-tree-pop (persistent-vector-tree p-vec) (persistent-vector-height p-vec))
    (values (if (eq new-tree *empty-array*)
                *empty-persistent-vector*
                (copy-persistent-vector p-vec :tree new-tree :height new-height :count (1- (persistent-vector-count p-vec))))
            popped-value
            t)))

(defun vector-tree-aref (vec height index)
  (cond
    ((equal height 1)
     (if (>= index (length vec))
         (error "Index out of bounds")
         (aref vec index)))

    (t
     (multiple-value-bind (child-index next-index) (floor index (expt +branching-factor+ (1- height)))
       (if (>= child-index (length vec))
           (error "Index out of bounds")
           (vector-tree-aref (aref vec child-index) (1- height) next-index))))))

(defun persistent-vector-lookup (p-vec index)
  (vector-tree-aref (persistent-vector-tree p-vec) (persistent-vector-height p-vec) index))

(defun vector-tree-update (vec height index new-value)
  (cond
    ((equal height 1)
     (if (>= index (length vec))
         (error "Index out of bounds")
         (vector-with-update vec new-value index)))

    (t
     (multiple-value-bind (child-index next-index) (floor index (expt +branching-factor+ (1- height)))
       (if (>= child-index (length vec))
           (error "Index out of bounds")
           (vector-with-update vec (vector-tree-update (aref vec child-index) (1- height) next-index new-value) child-index))))))

(defun persistent-vector-with-update (p-vec index new-value)
  (let ((new-tree (vector-tree-update (persistent-vector-tree p-vec) (persistent-vector-height p-vec) index new-value)))
    (if (eq new-tree (persistent-vector-tree p-vec))
        p-vec
        (copy-persistent-vector p-vec :tree new-tree))))

(defmethod with-head ((p-vec persistent-vector) object)
  (persistent-vector-push p-vec object))

(defmethod head ((p-vec persistent-vector))
  (if (zerop (persistent-vector-count p-vec))
      (values nil nil)
      (values (persistent-vector-lookup p-vec (1- (persistent-vector-count p-vec))) t)))

(defmethod tail ((p-vec persistent-vector))
  (persistent-vector-pop p-vec))

(defmethod is-empty ((p-vec persistent-vector))
  (zerop (persistent-vector-count p-vec)))

(defmethod empty ((p-vec persistent-vector))
  *empty-persistent-vector*)

(defun persistent-vector-pour (recipient donor donor-start-index donor-end-index)
  (loop :for i :from donor-start-index :below donor-end-index :do
    (setf recipient (persistent-vector-push recipient (persistent-vector-lookup donor i))))
  recipient)

(defmethod without-entry ((p-vec persistent-vector) index)
  (check-type index (integer 0))
  (let ((count (persistent-vector-count p-vec)))
    (when (or (>= index count)
              (< index 0))
      (error "index out of bounds: ~A" index))

    (cond
      ((zerop index)
       (values (subsequence p-vec 1 count)
               (persistent-vector-lookup p-vec 0)
               t))

      ((equal index (1- (persistent-vector-count p-vec)))
       (persistent-vector-pop p-vec))

      (t
       (persistent-vector-pour (subsequence p-vec 0 index) p-vec (1+ index) count)))))

(defmethod sequence-insert ((p-vec persistent-vector) before-index object)
  (check-type before-index (integer 0))
  (cond
    ((equal before-index (persistent-vector-count p-vec))
     (persistent-vector-push p-vec object))
    ((< before-index (persistent-vector-count p-vec))
     (let ((result (subsequence p-vec 0 before-index)))
       (setf result (persistent-vector-push result object))
       (persistent-vector-pour result p-vec before-index (persistent-vector-count p-vec))))
    (t
     (error "before-index is out of bounds: ~A" before-index))))

(defmethod concatenate-sequences ((p-vec persistent-vector) other)
  (when (and (zerop (persistent-vector-count p-vec))
             (persistent-vector-p other))
    (return-from concatenate-sequences other))

  (loop :with iter = (iterator other) :do
    (multiple-value-bind (value valid-p) (funcall iter)
      (if valid-p
          (setf p-vec (persistent-vector-push p-vec value))
          (return))))

  p-vec)

(defmethod subsequence ((p-vec persistent-vector) min max)
  (check-type min (integer 0))
  (check-type max (or null (integer 0)))
  (let ((count (persistent-vector-count p-vec)))
    (unless max
      (setf max count))

    (when (> max count)
      (error "max is out of bounds: ~A" max))

    (when (< max min)
      (error "bounds are invalid: min ~A, max ~A" min max))

    (cond
      ((equal min max)
       *empty-persistent-vector*)

      ((and (zerop min)
            (equal max count))
       p-vec)

      ((zerop min)
       (let ((elements-to-cut (- count max))
             (expected-length (- max min)))
         (cond
           ((> elements-to-cut expected-length)
            (persistent-vector-pour *empty-persistent-vector* p-vec min max))
           (t
            (dotimes (i elements-to-cut)
              (setf p-vec (persistent-vector-pop p-vec)))
            p-vec))))

      (t
       (persistent-vector-pour *empty-persistent-vector* p-vec min max)))))

(defmethod with-entry ((p-vec persistent-vector) key value)
  (check-type key (integer 0))
  (cond
    ((> key (persistent-vector-count p-vec))
     (error "Index out of bounds"))
    ((equal key (persistent-vector-count p-vec))
     (persistent-vector-push p-vec value))
    (t
     (persistent-vector-with-update p-vec key value))))

(defmethod lookup-entry ((p-vec persistent-vector) key)
  (check-type key (integer 0))
  (if (>= key (persistent-vector-count p-vec))
      (values nil nil)
      (values (persistent-vector-lookup p-vec key) t)))

(defun do-vector-tree-f (vec height fn index)
  (cond
    ((equal height 1)
     (loop :for object :across vec
           :for offset :from index
           :do
              (funcall fn offset object)))
    (t
     (loop :for object :across vec
           :for offset = index :then (+ offset (expt +branching-factor+ (1- height))) :do
             (do-vector-tree-f object (1- height) fn offset)))))

(defun persistent-vector-for-each-kv (p-vec function)
  (when (zerop (persistent-vector-count p-vec))
    (return-from persistent-vector-for-each-kv))

  (let ((tree (persistent-vector-tree p-vec))
        (height (persistent-vector-height p-vec)))
    (do-vector-tree-f tree height function 0)))

(defmethod for-each ((p-vec persistent-vector) function)
  (persistent-vector-for-each-kv p-vec
                                 (lambda (k v)
                                   (declare (ignore k))
                                   (funcall function v))))

(defmethod for-each-kv ((p-vec persistent-vector) function)
  (persistent-vector-for-each-kv p-vec function))

(defun make-persistent-vector-iterator (p-vec)
  (let ((offset 0))
    (lambda ()
      ;; This won't be O(1) amortized, but its MUCH easier to write!
      (cond
        ((>= offset (persistent-vector-count p-vec))
         (values nil nil))

        (t
         (let ((object (persistent-vector-lookup p-vec offset)))
           (incf offset)
           (values object t)))))))

(defmethod iterator ((p-vec persistent-vector))
  (make-persistent-vector-iterator p-vec))

(defmethod print-object ((p-vec persistent-vector) stream)
  (if *print-readably*
      (call-next-method)
      (print-sequence p-vec stream)))

(defun make-persistent-vector (&key items)
  (let ((result *empty-persistent-vector*))
    (dolist (item items)
      (setf result (persistent-vector-push result item)))
    result))

(defun persistent-vector (&rest items)
  (make-persistent-vector :items items))

(defmethod size ((p-vec persistent-vector))
  (persistent-vector-count p-vec))

(defmethod compare-objects ((left persistent-vector) (right persistent-vector))
  (compare-containers left right #'compare))
