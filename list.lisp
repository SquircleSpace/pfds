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

(defpackage :pfds.shcl.io/list
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common
   #:define-interface #:is-empty #:empty #:define-adt #:to-list)
  (:import-from :pfds.shcl.io/compare
   #:compare #:compare* #:compare-objects)
  (:export
   #:with-head #:head #:tail #:is-empty #:empty #:empty-pure-list
   #:empty-list #:update #:pure-list #:pure-list* #:pure-list-cons
   #:list-append #:list-take #:list-reverse))
(in-package :pfds.shcl.io/list)

(define-interface list
  (defgeneric with-head (item list))
  (defgeneric head (list))
  (defgeneric tail (list))
  is-empty
  empty)

(defun list-append (left-list right-list)
  (if (is-empty left-list)
      right-list
      (with-head (head left-list) (list-append (tail left-list) right-list))))

(defun list-take (list count)
  (check-type count (integer 0))
  (cond
    ((is-empty list)
     list)
    ((zerop count)
     (empty list))
    (t
     (with-head (head list) (list-take (tail list) (1- count))))))

(defun list-drop (list count)
  (check-type count (integer 0))
  (cond
    ((or (is-empty list)
         (zerop count))
     list)
    (t
     (list-drop (tail list) (1- count)))))

(defun list-reverse (list)
  (let ((result (empty list)))
    (loop :while (not (is-empty list)) :do
          (let ((head (head list)))
            (setf result (with-head head result))
            (setf list (tail list))))
    result))

(defun update (list index value)
  (cond
    ((is-empty list)
     (error "Invalid subscript"))
    ((zerop index)
     (with-head value (tail list)))
    ((plusp index)
     (with-head (head list) (update (tail list) (1- index) value)))
    (t
     (error "Invalid subscript"))))

(define-adt pure-list
    ()
  (pure-list-nil)
  ((pure-list-cons (:constructor %make-pure-list-cons))
   head
   (tail (empty-pure-list) :type pure-list)))

(defvar *empty-pure-list*
  (make-pure-list-nil))

(defun empty-pure-list ()
  *empty-pure-list*)

(defun pure-list* (&rest objects)
  (labels
      ((visit (objects)
         (if (cdr objects)
             (with-head (car objects) (visit (cdr objects)))
             (if (car objects)
                 (visit (car objects))
                 (empty-pure-list)))))
    (if (null objects)
        (empty-pure-list)
        (visit objects))))

(defun pure-list (&rest objects)
  (pure-list* objects nil))

(defun pure-list-cons (head tail)
  (check-type tail pure-list)
  (%make-pure-list-cons :head head :tail tail))

(defmethod to-list ((list pure-list-nil))
  nil)

(defmethod to-list ((list pure-list-cons))
  (cons (pure-list-cons-head list) (to-list (pure-list-cons-tail list))))

(defmethod with-head (item (list pure-list))
  (pure-list-cons list item))

(defmethod head ((list pure-list-cons))
  (values (pure-list-cons-head list) t))

(defmethod head ((list pure-list-nil))
  (values nil nil))

(defmethod tail ((list pure-list-cons))
  (values (pure-list-cons-tail list) t))

(defmethod tail ((list pure-list-nil))
  (values list nil))

(defmethod is-empty ((list pure-list-cons))
  nil)

(defmethod is-empty ((list pure-list-nil))
  t)

(defmethod empty ((list pure-list-cons))
  (empty-pure-list))

(defmethod empty ((list pure-list-nil))
  list)

(defun compare-pure-list (left right &key (head-compare-fn 'compare))
  (when (eql left right)
    (return-from compare-pure-list :equal))

  (cond
    ((and (pure-list-nil-p left)
          (pure-list-nil-p right))
     :equal)
    ((pure-list-nil-p left)
     :less)
    ((pure-list-nil-p right)
     :greater)
    (t
     (compare*
       (funcall head-compare-fn (pure-list-cons-head left) (pure-list-cons-head right))
       (compare-pure-list (pure-list-cons-tail left) (pure-list-cons-tail right)
                          :head-compare-fn head-compare-fn)))))

(defmethod compare-objects ((left pure-list-nil) (right pure-list-nil))
  :equal)

(defmethod compare-objects ((left pure-list-cons) (right pure-list-cons))
  (compare-pure-list left right))

(defmethod compare-objects ((left pure-list-nil) (right pure-list-cons))
  :less)

(defmethod compare-objects ((left pure-list-cons) (right pure-list-nil))
  :greater)

(defmethod with-head (item (list list))
  (cons item list))

(defmethod head ((list list))
  (if list
      (values (car list) t)
      (values nil nil)))

(defmethod tail ((list list))
  (if list
      (values (cdr list) t)
      (values nil nil)))

(defmethod is-empty ((list list))
  (null list))

(defmethod empty ((list list))
  nil)

(defun empty-list ()
  nil)

(defmethod to-list ((list list))
  list)
