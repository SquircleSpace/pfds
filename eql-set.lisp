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

(defpackage :pfds.shcl.io/eql-set
  (:use :common-lisp)
  (:export
   #:make-eql-set
   #:eql-set-p
   #:eql-set
   #:eql-set-with
   #:eql-set-without
   #:eql-set-contains-p
   #:eql-set-count
   #:eql-set-union
   #:eql-set-intersection
   #:eql-set-difference
   #:eql-set-representative
   #:do-eql-set))
(in-package :pfds.shcl.io/eql-set)

(defstruct (eql-set (:constructor %make-eql-set) (:copier %copy-eql-set))
  (set (make-hash-table :test 'eql)))

(defun make-eql-set (&rest items)
  (let ((new-table (make-hash-table :test 'eql)))
    (dolist (item items)
      (setf (gethash item new-table) t))
    (%make-eql-set :set new-table)))

(defun copy-hash-table (hash-table)
  (let ((new-table (make-hash-table :test (hash-table-test hash-table)
                                    :size (hash-table-size hash-table)
                                    :rehash-size (hash-table-rehash-size hash-table)
                                    :rehash-threshold (hash-table-rehash-threshold hash-table))))
    (loop :for key :being :the :hash-keys :of hash-table
          :using (:hash-value value) :do
            (setf (gethash key new-table) value))
    new-table))

(defun copy-eql-set (eql-set)
  (%make-eql-set :set (copy-hash-table (eql-set-set eql-set))))

(defun eql-set-with (eql-set item)
  (when (nth-value 1 (gethash item (eql-set-set eql-set)))
    (return-from eql-set-with eql-set))

  (let ((new-set (copy-eql-set eql-set)))
    (setf (gethash item (eql-set-set new-set)) t)
    new-set))

(defun eql-set-without (eql-set item)
  (unless (nth-value 1 (gethash item (eql-set-set eql-set)))
    (return-from eql-set-without eql-set))

  (let ((new-set (copy-eql-set eql-set)))
    (remhash item (eql-set-set new-set))
    new-set))

(defun eql-set-contains-p (eql-set item)
  (nth-value 1 (gethash item (eql-set-set eql-set))))

(defun eql-set-count (eql-set)
  (hash-table-count (eql-set-set eql-set)))

(defun eql-set-union (first second)
  (when (eql first second)
    (return-from eql-set-union first))

  (multiple-value-bind (larger smaller)
      (if (>= (hash-table-count (eql-set-set first))
              (hash-table-count (eql-set-set second)))
          (values first second)
          (values second first))
    (let ((new-table (copy-hash-table (eql-set-set larger))))
      (loop :for key :being :the :hash-keys :of (eql-set-set smaller) :do
        (setf (gethash key new-table) t))
      (%make-eql-set :set new-table))))

(defun eql-set-intersection (first second)
  (when (eql first second)
    (return-from eql-set-intersection first))

  (multiple-value-bind (larger smaller)
      (if (>= (hash-table-count (eql-set-set first))
              (hash-table-count (eql-set-set second)))
          (values first second)
          (values second first))
    (let ((new-table (make-hash-table :test 'eql)))
      (loop :for key :being :the :hash-keys :of (eql-set-set smaller) :do
        (when (nth-value 1 (gethash key (eql-set-set larger)))
          (setf (gethash key new-table) t)))
      (%make-eql-set :set new-table))))

(defun eql-set-difference (first second)
  (when (eql first second)
    (return-from eql-set-difference (make-eql-set)))

  (let ((new-table (make-hash-table :test 'eql)))
    (loop :for key :being :the :hash-keys :of (eql-set-set first) :do
      (unless (nth-value 1 (gethash key (eql-set-set second)))
        (setf (gethash key new-table) t)))
    (%make-eql-set :set new-table)))

(defun eql-set-representative (eql-set)
  (loop :for key :being :the :hash-keys :of (eql-set-set eql-set) :do
    (return-from eql-set-representative (values key t)))
  (values nil nil))

(defmacro do-eql-set ((member eql-set &optional result) &body body)
  `(loop :for ,member :being :the :hash-keys :of (eql-set-set ,eql-set)
         :do (progn ,@body)
         :finally (return ,result)))
