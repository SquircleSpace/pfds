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
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-containers)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-sequence)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc #:cassert)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:export
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

(declare-interface-conformance persistent-vector sequence)

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
      (values p-vec nil nil)))

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
     (if (or (>= index (length vec))
             (minusp index))
         (values nil nil)
         (values (aref vec index) t)))

    (t
     (multiple-value-bind (child-index next-index) (floor index (expt +branching-factor+ (1- height)))
       (if (>= child-index (length vec))
           (values nil nil)
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

(defmethod with-top ((p-vec persistent-vector) object)
  (persistent-vector-push p-vec object))

(defmethod peek-top ((p-vec persistent-vector))
  (if (zerop (persistent-vector-count p-vec))
      (values nil nil)
      (values (persistent-vector-lookup p-vec (1- (persistent-vector-count p-vec))) t)))

(defmethod without-top ((p-vec persistent-vector))
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
      (return-from without-entry
        (values p-vec nil nil)))

    (cond
      ((zerop index)
       (values (subsequence p-vec 1 count)
               (persistent-vector-lookup p-vec 0)
               t))

      ((equal index (1- (persistent-vector-count p-vec)))
       (persistent-vector-pop p-vec))

      (t
       (values (persistent-vector-pour (subsequence p-vec 0 index) p-vec (1+ index) count)
               (persistent-vector-lookup p-vec index)
               t)))))

(defun persistent-vector-insert (p-vec before-index object)
  (check-type before-index (integer 0))
  (cond
    ((>= before-index (persistent-vector-count p-vec))
     (dotimes (i (- before-index (persistent-vector-count p-vec)))
       (setf p-vec (persistent-vector-push p-vec nil)))
     (persistent-vector-push p-vec object))

    (t
     (let ((result (subsequence p-vec 0 before-index)))
       (setf result (persistent-vector-push result object))
       (persistent-vector-pour result p-vec before-index (persistent-vector-count p-vec))))))

(defmethod insert ((p-vec persistent-vector) before-index object)
  (persistent-vector-insert p-vec before-index object))

(defmethod join ((p-vec persistent-vector) other)
  (when (and (zerop (persistent-vector-count p-vec))
             (persistent-vector-p other))
    (return-from join other))

  (loop :with iter = (iterator other) :do
    (multiple-value-bind (value valid-p) (funcall iter)
      (if valid-p
          (setf p-vec (persistent-vector-push p-vec value))
          (return))))

  p-vec)

(defun persistent-vector-subsequence (p-vec min max)
  (check-type min (integer 0))
  (check-type max (or null (integer 0)))
  (let ((count (persistent-vector-count p-vec)))
    (unless max
      (setf max count))

    (when (> max count)
      (setf max count))

    (cond
      ((>= min max)
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

(defmethod subsequence ((p-vec persistent-vector) min max)
  (persistent-vector-subsequence p-vec min max))

(defmethod with-entry ((p-vec persistent-vector) key value)
  (check-type key (integer 0))
  (cond
    ((< key (persistent-vector-count p-vec))
     (persistent-vector-with-update p-vec key value))
    (t
     (dotimes (i (- (1- key) (persistent-vector-count p-vec)))
       (setf p-vec (persistent-vector-push p-vec nil)))
     (persistent-vector-push p-vec value))))

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

(defun map-vector-tree-f (vec height fn index)
  (let ((here-function (if (equal height 1)
                           fn
                           (lambda (offset value)
                             (map-vector-tree-f value (1- height) fn offset)))))
    (loop :with result = nil
          :with offset-step = (expt +branching-factor+ (1- height))
          :for object :across vec
          :for offset = index :then (+ offset offset-step)
          :for here-index :from 0
          :for new-object = (funcall here-function offset object) :do
            (cond
              (result
               (setf (aref result here-index) new-object))
              ((not (eql new-object object))
               (setf result (make-vector vec))
               (setf (aref result here-index) new-object)))
          :finally (return (or result vec)))))

(defun persistent-vector-map-entries (p-vec function)
  (when (zerop (persistent-vector-count p-vec))
    (return-from persistent-vector-map-entries p-vec))

  (let ((tree (persistent-vector-tree p-vec))
        (height (persistent-vector-height p-vec)))
    (copy-persistent-vector p-vec :tree (map-vector-tree-f tree height function 0))))

(defun persistent-vector-for-each-entry (p-vec function)
  (when (zerop (persistent-vector-count p-vec))
    (return-from persistent-vector-for-each-entry))

  (let ((tree (persistent-vector-tree p-vec))
        (height (persistent-vector-height p-vec)))
    (do-vector-tree-f tree height function 0)))

(defmethod for-each ((p-vec persistent-vector) function)
  (persistent-vector-for-each-entry p-vec
                                    (lambda (k v)
                                      (declare (ignore k))
                                      (funcall function v))))

(defmethod for-each-entry ((p-vec persistent-vector) function)
  (persistent-vector-for-each-entry p-vec function))

(defmethod map-entries ((p-vec persistent-vector) function)
  (persistent-vector-map-entries p-vec function))

(defmethod map-members ((p-vec persistent-vector) function)
  (persistent-vector-map-entries p-vec (lambda (k v)
                                         (declare (ignore k))
                                         (funcall function v))))

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

(defmethod with-member ((p-vec persistent-vector) object)
  (persistent-vector-push p-vec object))

(defmethod decompose ((p-vec persistent-vector))
  (persistent-vector-pop p-vec))

(defun persistent-vector-with-back (p-vec object)
  (persistent-vector-push p-vec object))

(defun persistent-vector-without-back (p-vec)
  (persistent-vector-pop p-vec))

(defun persistent-vector-peek-back (p-vec)
  (persistent-vector-lookup p-vec (1- (persistent-vector-count p-vec))))

(defun persistent-vector-with-front (p-vec object)
  (persistent-vector-insert p-vec 0 object))

(defun persistent-vector-without-front (p-vec)
  (if (zerop (persistent-vector-count p-vec))
      (values p-vec nil nil)
      (values (persistent-vector-subsequence p-vec 1 nil)
              (persistent-vector-lookup p-vec 0)
              t)))

(defun persistent-vector-peek-front (p-vec)
  (persistent-vector-lookup p-vec 0))

(defmethod with-back ((p-vec persistent-vector) object)
  (persistent-vector-with-back p-vec object))

(defmethod without-back ((p-vec persistent-vector))
  (persistent-vector-without-back p-vec))

(defmethod peek-back ((p-vec persistent-vector))
  (persistent-vector-peek-back p-vec))

(defmethod with-front ((p-vec persistent-vector) object)
  (persistent-vector-with-front p-vec object))

(defmethod without-front ((p-vec persistent-vector))
  (persistent-vector-without-front p-vec))

(defmethod peek-front ((p-vec persistent-vector))
  (persistent-vector-peek-front p-vec))

(defmethod with-first ((p-vec persistent-vector) object)
  (persistent-vector-with-front p-vec object))

(defmethod without-first ((p-vec persistent-vector))
  (persistent-vector-without-front p-vec))

(defmethod peek-first ((p-vec persistent-vector))
  (persistent-vector-peek-front p-vec))

(defmethod with-last ((p-vec persistent-vector) object)
  (persistent-vector-with-back p-vec object))

(defmethod without-last ((p-vec persistent-vector))
  (persistent-vector-without-back p-vec))

(defmethod peek-last ((p-vec persistent-vector))
  (persistent-vector-peek-back p-vec))

(defmethod with-top ((p-vec persistent-vector) object)
  (persistent-vector-with-back p-vec object))

(defmethod without-top ((p-vec persistent-vector))
  (persistent-vector-without-back p-vec))

(defmethod peek-top ((p-vec persistent-vector))
  (persistent-vector-peek-back p-vec))

(defun vector-tree-print-graphviz (thing height stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (when (zerop height)
      (format stream "ID~A [shape=box, label=\"~A\"]~%" id thing)
      (return-from vector-tree-print-graphviz id))

    (format stream "ID~A [shape=oval, label=\"\"]~%" id)

    (loop :for child :across thing :do
      (let ((child-id (vector-tree-print-graphviz child (1- height) stream id-vendor)))
        (format stream "ID~A -> ID~A~%" id child-id)))

    id))

(defmethod print-graphviz ((p-vec persistent-vector) stream id-vendor)
  (vector-tree-print-graphviz (persistent-vector-tree p-vec) (persistent-vector-height p-vec)
                              stream id-vendor))

(defun check-vector-tree (tree height partial-fill-okay)
  (when (zerop height)
    (return-from check-vector-tree 1))

  (cassert (typep tree 'vector) (tree)
           "Tree levels must be vectors")

  (unless partial-fill-okay
    (cassert (equal +branching-factor+ (length tree)) (tree)
             "Trees at this point must be full"))

  (let ((count 0))
    (when (plusp (length tree))
      (loop :for index :below (1- (length tree)) :do
        (let ((element (aref tree index)))
          (incf count (check-vector-tree element (1- height) nil))))

      (incf count (check-vector-tree (aref tree (1- (length tree))) (1- height) partial-fill-okay)))

    count))

(defmethod check-invariants ((p-vec persistent-vector))
  (let ((count (check-vector-tree
                (persistent-vector-tree p-vec)
                (persistent-vector-height p-vec)
                t)))
    (cassert (equal count (persistent-vector-count p-vec)) (p-vec count)
             "The persistent vector's count must be accurate.")))
