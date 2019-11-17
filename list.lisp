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
  (:import-from :pfds.shcl.io/common #:define-interface #:is-empty #:empty #:define-adt #:compare)
  (:export
   #:with-head #:head #:tail #:is-empty #:empty #:empty-pure-list
   #:empty-list #:list-+ #:update))
(in-package :pfds.shcl.io/list)

(define-interface list
  (defgeneric with-head (item list))
  (defgeneric head (list))
  (defgeneric tail (list))
  is-empty
  empty)

(defun list-+ (left-list right-list)
  (if (is-empty left-list)
      right-list
      (with-head (head left-list) (list-+ (tail left-list) right-list))))

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

(define-adt %pure-list
    ()
  (%empty-pure-list)
  (%nonempty-pure-list
   head
   (tail (empty-pure-list))))

(defconstant +empty-pure-list+
  (if (boundp '+empty-pure-list+)
      (symbol-value '+empty-pure-list+)
      (make-%empty-pure-list)))

(defun empty-pure-list ()
  +empty-pure-list+)

(defun pure-list* (objects)
  (if objects
      (with-head (car objects) (pure-list* (cdr objects)))
      (empty-pure-list)))

(defun pure-list (&rest objects)
  (pure-list* objects))

(defmethod with-head (item (list %pure-list))
  (make-%nonempty-pure-list :head item :tail list))

(defmethod head ((list %nonempty-pure-list))
  (%nonempty-pure-list-head list))

(defmethod head ((list %empty-pure-list))
  (error "List is empty"))

(defmethod tail ((list %nonempty-pure-list))
  (%nonempty-pure-list-tail list))

(defmethod tail ((list %empty-pure-list))
  (error "List is empty"))

(defmethod is-empty ((list %nonempty-pure-list))
  nil)

(defmethod is-empty ((list %empty-pure-list))
  t)

(defmethod empty ((list %pure-list))
  (empty-pure-list))

(defmethod compare ((left %empty-pure-list) (right %empty-pure-list))
  t)

(defmethod compare ((left %nonempty-pure-list) (right %nonempty-pure-list))
  (when (eql left right)
    (return-from compare :equal))
  (compare))

(defmethod with-head (item (list list))
  (cons item list))

(defmethod head ((list list))
  (car list))

(defmethod tail ((list list))
  (cdr list))

(defmethod is-empty ((list list))
  (null list))

(defmethod empty ((list list))
  nil)

(defun empty-list ()
  nil)
