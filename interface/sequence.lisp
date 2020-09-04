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

(defpackage :pfds.shcl.io/interface/sequence
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:is-empty #:empty #:define-interface
   #:with-entry #:lookup-entry #:without-entry
   #:to-list)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder
   #:impure-list-builder-add #:impure-list-builder-extract)
  (:export
   #:sequence
   #:is-empty
   #:empty
   #:with-entry
   #:without-entry
   #:lookup-entry
   #:sequence-insert
   #:concatenate-sequences
   #:subsequence))
(in-package :pfds.shcl.io/interface/sequence)

(define-interface sequence
  is-empty
  empty
  with-entry
  lookup-entry
  without-entry

  (defgeneric sequence-insert (sequence before-index object))
  (defgeneric concatenate-sequences (left right))
  (defgeneric subsequence (seq min max)))

(defmethod with-entry ((list null) index value)
  (unless (zerop index)
    (error "index out of bounds: ~A" index))
  (cons value nil))

(defmethod with-entry ((list cons) index value)
  (when (zerop index)
    (return-from with-entry
      (cons value (cdr list))))

  (let ((builder (make-impure-list-builder)))
    (dotimes (i index)
      (unless list
        (error "index out of bounds: ~A" index))
      (impure-list-builder-add builder (pop list)))
    (impure-list-builder-add builder value)
    (pop list)
    (impure-list-builder-extract list)))

(defmethod lookup-entry ((list list) index)
  (dotimes (i index)
    (unless list
      (return-from lookup-entry
        (values nil nil)))
    (setf list (cdr list)))
  (if list
      (values (car list) t)
      (values nil nil)))

(defmethod without-entry ((list null) index)
  (error "index out of bounds: ~A" index))

(defmethod without-entry ((list cons) index)
  (when (zerop index)
    (return-from without-entry
      (cdr list)))

  (let ((builder (make-impure-list-builder)))
    (dotimes (i index)
      (unless list
        (error "index out of bounds: ~A" index))
      (impure-list-builder-add builder (pop list)))
    (pop list)
    (impure-list-builder-extract list)))

(defun list-insert (list index object)
  (when (zerop index)
    (return-from list-insert
      (cons object list)))

  (let ((builder (make-impure-list-builder)))
    (dotimes (i index)
      (unless list
        (error "index out of bounds: ~A" index))
      (impure-list-builder-add builder (pop list)))
    (impure-list-builder-add builder object)
    (impure-list-builder-extract list)))

(defmethod sequence-insert ((list list) index object)
  (check-type index (integer 0))
  (list-insert list index object))

(defmethod concatenate-sequences ((left list) other)
  (unless (listp other)
    (setf other (to-list other)))
  (append left other))

(defmethod subsequence ((seq list) min max)
  (subseq seq min max))
