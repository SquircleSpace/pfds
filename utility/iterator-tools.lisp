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

(defpackage :pfds.shcl.io/utility/iterator-tools
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:iterator #:size)
  (:import-from :pfds.shcl.io/interface/heap
   #:without-heap-top)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare* #:compare-reals #:compare)
  (:export
   #:iterator-flatten #:iterator-flatten*
   #:compare-iterator-contents
   #:compare-containers #:compare-heaps #:compare-sets
   #:compare-maps))
(in-package :pfds.shcl.io/utility/iterator-tools)

(defun iterator-flatten (iterator)
  (let (current-iterator)
    (labels
        ((iterate ()
           (loop
             (unless current-iterator
               (multiple-value-bind (new-iterator valid-p) (funcall iterator)
                 (unless valid-p
                   (return-from iterate (values nil nil)))
                 (assert new-iterator)
                 (setf current-iterator new-iterator)))
             (multiple-value-bind (result valid-p) (funcall current-iterator)
               (if valid-p
                   (return-from iterate (values result valid-p))
                   (setf current-iterator nil))))))
      #'iterate)))

(defun iterator-flatten* (&rest iterators)
  (iterator-flatten (iterator iterators)))

(defun compare-iterator-contents (left right comparator)
  (let ((result :equal))
    (loop
      (multiple-value-bind (left-value left-valid-p) (funcall left)
        (multiple-value-bind (right-value right-valid-p) (funcall right)
          (cond
            ((and left-valid-p right-valid-p)
             (let ((comparison (funcall comparator left-value right-value)))
               (ecase comparison
                 (:equal)
                 (:unequal
                  (setf result :unequal))
                 ((:less :greater)
                  (return comparison)))))

            (left-valid-p
             (return :greater))
            (right-valid-p
             (return :less))
            (t
             (return result))))))))

(defun compare-containers (left-iterable right-iterable comparator)
  (compare*
    (compare-reals (size left-iterable) (size right-iterable))
    (compare-iterator-contents (iterator left-iterable) (iterator right-iterable) comparator)))

(defun heap-iterator (heap)
  (lambda ()
    (multiple-value-bind (new-heap removed-value valid-p) (without-heap-top heap)
      (setf heap new-heap)
      (values removed-value valid-p))))

(defun nil-instead-of-unequal (value)
  (unless (eq :unequal value)
    value))

(defun compare-heaps (left-heap left-comparator right-heap right-comparator &optional (comparator-comparator #'compare))
  (compare*
    (compare-reals (size left-heap) (size right-heap))
    (or (nil-instead-of-unequal (funcall comparator-comparator left-comparator right-comparator))
        (return-from compare-heaps :unequal))
    (compare-iterator-contents (heap-iterator left-heap) (heap-iterator right-heap) left-comparator)))

(defun compare-sets (left-set left-comparator right-set right-comparator &optional (comparator-comparator #'compare))
  (compare*
    (compare-reals (size left-set) (size right-set))
    (or (nil-instead-of-unequal (funcall comparator-comparator left-comparator right-comparator))
        (return-from compare-sets :unequal))
    (compare-iterator-contents (iterator left-set) (iterator right-set) left-comparator)))

(defun compare-maps (left-map left-comparator right-map right-comparator
                     &optional (comparator-comparator #'compare)
                       (value-comparator #'compare))
  (let (comparator-comparison)
    (compare*
      (compare-reals (size left-map) (size right-map))
      (setf comparator-comparison (funcall comparator-comparator left-comparator right-comparator))
      (compare-iterator-contents (iterator left-map) (iterator right-map)
                                 (if (eq :equal comparator-comparison)
                                     (lambda (l-pair r-pair)
                                       (compare* (funcall left-comparator (car l-pair) (car r-pair))
                                                 (funcall value-comparator (cdr l-pair) (cdr r-pair))))
                                     (lambda (l-pair r-pair)
                                       (funcall value-comparator (cdr l-pair) (cdr r-pair))))))))
