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

(defpackage :pfds.shcl.io/stream
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/lazy #:force #:lazy)
  (:import-from :pfds.shcl.io/list
   #:with-head #:head #:tail #:is-empty #:empty)
  (:import-from :pfds.shcl.io/common
   #:define-adt #:to-list)
  (:export
   #:lazy-cons
   #:lazy-nil
   #:lazy-list
   #:with-head
   #:head
   #:tail
   #:is-empty
   #:empty
   #:stream-append))
(in-package :pfds.shcl.io/stream)

(defvar *empty-lazy-list*)

(defun empty-lazy-list ()
  *empty-lazy-list*)

(define-adt lazy-list
    ()
  (lazy-nil)
  ((lazy-cons (:constructor %make-lazy-cons))
   head
   (tail 'empty-lazy-list)))

(defvar *empty-lazy-list*
  (make-lazy-nil))

(defmacro lazy-cons (head &body tail-forms)
  `(%make-lazy-cons :head ,head :tail (lazy ,@tail-forms)))

(defmethod is-empty ((lazy-cons lazy-cons))
  nil)

(defmethod is-empty ((lazy-nil lazy-nil))
  t)

(defmethod empty ((lazy-cons lazy-cons))
  (empty-lazy-list))

(defmethod empty ((lazy-nil lazy-nil))
  lazy-nil)

(defmethod head ((lazy-cons lazy-cons))
  (values (lazy-cons-head lazy-cons) t))

(defmethod head ((lazy-nil lazy-nil))
  (values nil nil))

(defmethod tail ((lazy-cons lazy-cons))
  (values (force (lazy-cons-tail lazy-cons)) t))

(defmethod tail ((lazy-nil lazy-nil))
  (values lazy-nil nil))

(defmethod with-head (item (lazy-nil lazy-nil))
  (lazy-cons item lazy-nil))

(defmethod with-head (item (lazy-cons lazy-cons))
  (lazy-cons item lazy-cons))

(defmethod to-list ((lazy-cons lazy-cons))
  (cons (lazy-cons-head lazy-cons) (to-list (force (lazy-cons-tail lazy-cons)))))

(defmethod to-list ((lazy-nil lazy-nil))
  nil)

(defun stream-append (left right)
  (cond
    ((is-empty left)
     right)
    ((is-empty right)
     left)
    (t
     (lazy-cons (head left)
       (stream-append (tail left) right)))))
