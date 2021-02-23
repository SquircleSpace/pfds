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

(uiop:define-package :pfds.shcl.io/utility/iterator-tools
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/utility/specialization
   #:define-specializable-function)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare* #:compare-reals #:compare)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:export
   #:iterator-flatten #:iterator-flatten*
   #:iterator-to-list #:list-iterator
   #:singleton-iterator #:empty-iterator
   #:do-iterator
   #:compare-iterator-contents
   #:compare-collection-contents #:compare-heaps #:compare-ordered-sets
   #:compare-maps))
(in-package :pfds.shcl.io/utility/iterator-tools)

(defun list-iterator (list)
  (lambda ()
    (cond
      (list
       (let ((head (car list)))
         (setf list (cdr list))
         (values head t)))
      (t
       (values nil nil)))))

(defun singleton-iterator (item)
  (let ((valid-p t))
    (lambda ()
      (multiple-value-prog1
          (values item valid-p t)
        (setf valid-p nil)
        (setf item nil)))))

(defun empty-iterator ()
  (lambda ()
    (values nil nil)))

(defun iterator-flatten (iterator)
  (let (current-iterator)
    (labels
        ((iterate ()
           (loop
             (unless current-iterator
               (multiple-value-bind (new-iterator valid-p) (i-funcall iterator)
                 (unless valid-p
                   (return-from iterate (values nil nil)))
                 (assert new-iterator)
                 (setf current-iterator new-iterator)))
             (multiple-value-bind (result valid-p) (i-funcall current-iterator)
               (if valid-p
                   (return-from iterate (values result valid-p))
                   (setf current-iterator nil))))))
      #'iterate)))

(defun iterator-flatten* (&rest iterators)
  (iterator-flatten (list-iterator iterators)))

(defun iterator-to-list (iterator)
  (let ((builder (make-impure-list-builder)))
    (loop
      (multiple-value-bind (value valid-p) (i-funcall iterator)
        (if valid-p
            (impure-list-builder-add builder value)
            (return))))
    (impure-list-builder-extract builder)))

(defun compare-iterator-contents (left right comparator)
  (let ((result :equal))
    (loop
      (multiple-value-bind (left-value left-valid-p) (i-funcall left)
        (multiple-value-bind (right-value right-valid-p) (i-funcall right)
          (cond
            ((and left-valid-p right-valid-p)
             (let ((comparison (i-funcall comparator left-value right-value)))
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

(define-specializable-function compare-collection-contents
    (<interface>) (left right comparator)
  (with-interface <interface>
      (size iterator)
    (compare*
      (compare-reals (i-funcall size left)
                     (i-funcall size right))
      (compare-iterator-contents
       (i-funcall iterator left)
       (i-funcall iterator right)
       comparator))))

(define-specializable-function heap-iterator (<interface>) (heap)
  (with-interface <interface>
      (without-front)
    (lambda ()
      (multiple-value-bind (new-heap removed-value valid-p) (i-funcall without-front heap)
        (setf heap new-heap)
        (values removed-value valid-p)))))

(defun nil-instead-of-unequal (value)
  (unless (eq :unequal value)
    value))

(define-specializable-function compare-heaps
    (<interface>)
    (left right &optional (comparator-comparator #'compare))
  (with-interface <interface>
      (size comparator)
    (let ((left-comparator (i-funcall comparator left)))
      (compare*
        (compare-reals (i-funcall size left) (i-funcall size right))
        (or (nil-instead-of-unequal (i-funcall comparator-comparator
                                               left-comparator
                                               (i-funcall comparator right)))
            (return-from compare-heaps :unequal))
        (compare-iterator-contents
         (heap-iterator <interface> left)
         (heap-iterator <interface> right)
         left-comparator)))))

(define-specializable-function compare-ordered-sets
    (<interface>)
    (left right &optional (comparator-comparator #'compare))
  (with-interface <interface>
      (size comparator iterator)
    (let ((left-comparator (i-funcall comparator left)))
      (compare*
        (compare-reals (i-funcall size left) (i-funcall size right))
        (or (nil-instead-of-unequal (i-funcall comparator-comparator
                                               left-comparator
                                               (i-funcall comparator right)))
            (return-from compare-ordered-sets :unequal))
        (compare-iterator-contents
         (i-funcall iterator left)
         (i-funcall iterator right)
         left-comparator)))))

(define-specializable-function compare-ordered-maps
    (<interface>)
    (left right
          &optional (comparator-comparator #'compare)
          (value-comparator #'compare))
  (with-interface <interface>
      (size comparator iterator)
    (let (comparator-comparison
          (left-comparator (i-funcall comparator left)))
      (compare*
        (compare-reals (i-funcall size left) (i-funcall size right))
        (setf comparator-comparison (i-funcall comparator-comparator
                                               left-comparator
                                               (i-funcall comparator right)))
        (compare-iterator-contents
         (i-funcall iterator left)
         (i-funcall iterator right)
         (if (eq :equal comparator-comparison)
             (lambda (l-pair r-pair)
               (compare*
                 (i-funcall left-comparator (car l-pair) (car r-pair))
                 (i-funcall value-comparator (cdr l-pair) (cdr r-pair))))
             (lambda (l-pair r-pair)
               (i-funcall value-comparator (cdr l-pair) (cdr r-pair)))))))))

(defun do-iterator-f (iterator fn)
  (loop
    (multiple-value-bind (value valid-p) (funcall iterator)
      (unless valid-p
        (return))
      (funcall fn value))))

(defmacro do-iterator ((value iterator &optional result) &body body)
  `(block nil
     (do-iterator-f ,iterator (lambda (,value) ,@body))
     ,result))
