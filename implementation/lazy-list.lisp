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

(defpackage :pfds.shcl.io/implementation/lazy-list
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/utility/lazy #:force #:lazy)
  (:import-from :pfds.shcl.io/interface/list
   #:with-head #:head #:tail #:is-empty #:empty)
  (:import-from :pfds.shcl.io/interface/common
   #:to-list)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-adt)
  (:export
   #:make-lazy-list
   #:lazy-list
   #:lazy-list-p
   #:with-head
   #:head
   #:tail
   #:is-empty
   #:empty
   #:lazy-list-append
   #:lazy-list-reverse
   #:lazy-list-length))
(in-package :pfds.shcl.io/implementation/lazy-list)

;; See "Purely Functional Data Structures" by Chris Okasaki.

(defvar *empty-lazy-list*)

(defun empty-lazy-list ()
  *empty-lazy-list*)

(define-adt lazy-list
    ()
  (lazy-nil)
  ((lazy-cons (:constructor %make-lazy-cons))
   head
   (tail *empty-lazy-list*))
  (lazy-value
   (suspension (error "suspension is required"))))

(defvar *empty-lazy-list*
  (make-lazy-nil))

(defmacro lazy-cons (head tail)
  `(%make-lazy-cons :head ,head :tail ,tail))

(defmethod is-empty ((lazy-cons lazy-cons))
  nil)

(defmethod is-empty ((lazy-nil lazy-nil))
  t)

(defmethod is-empty ((lazy-value lazy-value))
  (is-empty (force (lazy-value-suspension lazy-value))))

(defmethod empty ((lazy-list lazy-list))
  (empty-lazy-list))

(defmethod head ((lazy-cons lazy-cons))
  (values (lazy-cons-head lazy-cons) t))

(defmethod head ((lazy-nil lazy-nil))
  (values nil nil))

(defmethod head ((lazy-value lazy-value))
  (head (force (lazy-value-suspension lazy-value))))

(defmethod tail ((lazy-cons lazy-cons))
  (values (lazy-cons-tail lazy-cons) (lazy-cons-head lazy-cons) t))

(defmethod tail ((lazy-nil lazy-nil))
  (values lazy-nil nil nil))

(defmethod tail ((lazy-value lazy-value))
  (tail (force (lazy-value-suspension lazy-value))))

(defmethod with-head ((lazy-list lazy-list) item)
  (%make-lazy-cons :head item :tail lazy-list))

(defmethod to-list ((lazy-cons lazy-cons))
  (cons (lazy-cons-head lazy-cons) (to-list (lazy-cons-tail lazy-cons))))

(defmethod to-list ((lazy-nil lazy-nil))
  nil)

(defmethod to-list ((lazy-value lazy-value))
  (to-list (force (lazy-value-suspension lazy-value))))

(defun lazy-list-append (left right)
  (cond
    ((is-empty left)
     right)
    ((is-empty right)
     left)
    (t
     (let* ((left-tail (tail left))
            (tail (make-lazy-value :suspension (lazy (lazy-list-append left-tail right)))))
       (lazy-cons (head left) tail)))))

(defmacro do-lazy-list ((value lazy-list &optional result) &body body)
  (let ((tip (gensym "TIP"))
        (new-tip (gensym "NEW-TIP"))
        (valid-p (gensym "VALID-P"))
        (head (gensym "HEAD")))
    `(loop :with ,tip = ,lazy-list :do
      (multiple-value-bind (,new-tip ,head ,valid-p) (tail ,tip)
        (unless ,valid-p
          (return ,result))
        (let ((,value ,head))
          ,@body)
        (setf ,tip ,new-tip)))))

(defun lazy-list-reverse (lazy-list)
  (make-lazy-value
   :suspension (lazy
                 (let ((result (empty-lazy-list)))
                   (do-lazy-list (value lazy-list)
                     (setf result (lazy-cons value result)))
                   result))))

(defun lazy-list-length (lazy-list)
  (let ((count 0))
    (do-lazy-list (value lazy-list)
      (declare (ignore value))
      (incf count))
    count))

(defun make-lazy-list (&key items)
  (labels
      ((visit (tail)
         (if tail
             (make-lazy-cons (car tail) (visit (cdr tail)))
             *empty-lazy-list*)))
    (visit items)))

(defun lazy-list (&rest items)
  (make-lazy-list :items items))
