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

(defpackage :pfds.shcl.io/list-utility
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add #:impure-list-builder-extract)
  (:export
   #:list-map-with
   #:list-map-without
   #:list-map-lookup
   #:list-set-with
   #:list-set-without
   #:list-set-is-member))
(in-package :pfds.shcl.io/list-utility)

(defun list-remove-index (list index)
  (let ((builder (make-impure-list-builder)))
    (dotimes (i index)
      (unless list
        (error "index is out of range: ~A" index))
      (impure-list-builder-add builder (pop list)))
    (unless list
      (error "index is out of range: ~A" index))
    (pop list)
    (impure-list-builder-extract builder list)))

(declaim (inline compare-equal-p))
(defun compare-equal-p (comparator left right)
  (eq :equal (funcall comparator left right)))

(defun list-map-with (comparator alist key value)
  (loop :for pair :in alist
        :for index :from 0
        :do
           (when (compare-equal-p comparator key (car pair))
             (if (eql (cdr pair) value)
                 (return-from list-map-with
                   alist)
                 (return
                   (setf alist (list-remove-index alist index))))))
  (cons (cons key value) alist))

(defun list-map-without (comparator alist key)
  (loop :for pair :in alist
        :for index :from 0
        :do
           (when (compare-equal-p comparator key (car pair))
             (return-from list-map-without
               (list-remove-index alist index))))
  alist)

(defun list-map-lookup (comparator alist key)
  (loop :for pair :in alist
        :do
        (when (compare-equal-p comparator key (car pair))
          (return-from list-map-lookup
            (values (cdr pair) t))))
  (values nil nil))

(defun list-set-with (comparator list key)
  (loop :for item :in list
        :do
           (when (compare-equal-p comparator key item)
             (return-from list-set-with list)))
  (cons key list))

(defun list-set-without (comparator list key)
  (loop :for item :in list
        :for index :from 0
        :do
        (when (compare-equal-p comparator key item)
          (return-from list-set-without
            (list-remove-index list index))))
  list)

(defun list-set-is-member (comparator list key)
  (loop :for item :in list
        :do
        (when (compare-equal-p comparator key item)
          (return-from list-set-is-member t)))
  nil)
