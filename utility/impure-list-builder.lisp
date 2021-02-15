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

(uiop:define-package :pfds.shcl.io/utility/impure-list-builder
  (:use :common-lisp)
  (:export
   #:make-impure-list-builder
   #:impure-list-builder
   #:impure-list-builder-add
   #:impure-list-builder-extract))
(in-package :pfds.shcl.io/utility/impure-list-builder)

(defstruct (impure-list-builder (:constructor %make-impure-list-builder))
  head
  tail)

(defun make-impure-list-builder ()
  (%make-impure-list-builder))

(defun impure-list-builder-add (list-builder item)
  (with-accessors
        ((head impure-list-builder-head)
         (tail impure-list-builder-tail))
      list-builder
    (if tail
        (setf (cdr tail) (cons item nil)
              tail (cdr tail))
        (setf head (cons item nil)
              tail head)))
  (values))

(defun impure-list-builder-extract (list-builder &optional tail)
  (with-accessors
        ((builder-tail impure-list-builder-tail))
      list-builder
    (if builder-tail
        (setf (cdr builder-tail) tail)
        (return-from impure-list-builder-extract tail)))

  (prog1 (impure-list-builder-head list-builder)
    (setf (impure-list-builder-head list-builder) nil)
    (setf (impure-list-builder-tail list-builder) nil)))
