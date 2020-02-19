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

(defpackage :pfds.shcl.io/implementation/pure-list
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:for-each #:size #:iterator)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-iterator-contents)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-adt)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare #:compare* #:compare-objects)
  (:import-from :pfds.shcl.io/interface/list
   #:with-head #:head #:tail #:is-empty #:empty)
  (:export
   #:with-head #:head #:tail #:is-empty #:empty
   #:pure-list #:make-pure-list #:pure-list-p))
(in-package :pfds.shcl.io/implementation/pure-list)

;; See "Purely Functional Data Structures" by Chris Okasaki

(defvar *empty-pure-list*)

(define-adt pure-list
    ()
  (pure-list-nil)
  (pure-list-cons
   head
   (tail *empty-pure-list* :type pure-list)))

(defvar *empty-pure-list*
  (make-pure-list-nil))

(defun make-pure-list (&key items)
  (labels
      ((visit (objects)
         (if objects
             (make-pure-list-cons :head (car objects) :tail (visit (cdr objects)))
             *empty-pure-list*)))
    (visit objects)))

(defun pure-list (&rest items)
  (make-pure-list :items objects))

(defmethod for-each ((list pure-list-nil) function)
  nil)

(defmethod for-each ((list pure-list-cons) function)
  (loop :until (pure-list-nil-p list) :do
    (progn
      (funcall function (pure-list-cons-head list))
      (setf list (pure-list-cons-tail list)))))

(defun make-pure-list-iterator (list)
  (lambda ()
    (cond
      ((pure-list-nil-p list)
       (values nil nil))

      (t
       (let ((result (pure-list-cons-head list)))
         (setf list (pure-list-cons-tail list))
         (values result t))))))

(defmethod iterator ((list pure-list))
  (make-pure-list-iterator list))

(defmethod with-head ((list pure-list) item)
  (make-pure-list-cons :head item :tail list))

(defmethod head ((list pure-list-cons))
  (values (pure-list-cons-head list) t))

(defmethod head ((list pure-list-nil))
  (values nil nil))

(defmethod tail ((list pure-list-cons))
  (values (pure-list-cons-tail list) (pure-list-cons-head list) t))

(defmethod tail ((list pure-list-nil))
  (values list nil nil))

(defmethod is-empty ((list pure-list-cons))
  nil)

(defmethod is-empty ((list pure-list-nil))
  t)

(defmethod empty ((list pure-list-cons))
  *empty-pure-list*)

(defmethod empty ((list pure-list-nil))
  list)

(defmethod compare-objects ((left pure-list) (right pure-list))
  (compare-iterator-contents (iterator left) (iterator right) #'compare))

(defmethod size ((list pure-list-nil))
  0)

(defmethod size ((list pure-list-cons))
  (let ((size 0))
    (loop :until (pure-list-nil-p list) :do
          (progn
            (incf size)
            (setf list (pure-list-cons-tail list))))
    size))
