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

(uiop:define-package :pfds.shcl.io
  (:use :common-lisp)
  (:use-reexport
   :pfds.shcl.io/interface/common
   :pfds.shcl.io/interface/heap
   :pfds.shcl.io/interface/map
   :pfds.shcl.io/interface/set)
  (:import-from :pfds.shcl.io/implementation/red-black-tree
   #:make-red-black-map
   #:make-red-black-set)
  (:import-from :pfds.shcl.io/implementation/leftist-heap
   #:make-leftist-heap)
  (:import-from :pfds.shcl.io/implementation/persistent-vector
   #:make-persistent-vector)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare)
  (:import-from :named-readtables
   #:defreadtable)
  (:export
   #:make-map
   #:make-set
   #:make-min-heap #:make-max-heap #:make-heap
   #:syntax
   #:compare))
(in-package :pfds.shcl.io)

(declaim (inline make-map))
(defun make-map (&key plist alist (comparator #'compare))
  "Make an ordered map.

The map will contain the associations described by the given alist and
plist.  If both the alist and the plist contain a key, then it is
unspecified which value will be contained in the map."
  (make-red-black-map comparator :plist plist :alist alist))

(declaim (inline make-set))
(defun make-set (&key items (comparator #'compare))
  "Make an ordered set."
  (make-red-black-set comparator :items items))

(declaim (inline make-min-heap))
(defun make-min-heap (&key items (comparator #'compare))
  "Make a min heap."
  (make-leftist-heap comparator :items items))

(declaim (inline make-max-heap))
(defun make-max-heap (&key items (comparator #'compare))
  "Make a max heap."
  (make-leftist-heap (lambda (l r) (funcall comparator r l)) :items items))

(declaim (inline make-heap))
(defun make-heap (&key (priority :min) items (comparator #'compare))
  "Make a heap."
  (check-type priority (member :min :max))
  (make-leftist-heap (if (eq priority :max)
                         (lambda (l r) (funcall comparator r l))
                         comparator)
                     :items items))

(defun make-vector (&key items)
  (make-persistent-vector :items items))

(defun read-vector (stream char)
  (declare (ignore char))
  (let ((forms (read-delimited-list #\] stream t)))
    `(make-vector :items (list ,@forms))))

(defun read-map (stream char)
  (declare (ignore char))
  (let ((pairs (read-delimited-list #\} stream t)))
    (labels
        ((clean (pair)
           (when (or (not (consp pair))
                     (null (cdr pair))
                     (cdr (cdr pair)))
             (error "Expected key/value pair, got ~A" pair))
           (destructuring-bind (key value) pair
             `(cons ,key ,value))))
      `(make-map :alist (list ,@(mapcar #'clean pairs))))))

(defun read-set (stream char arg)
  (declare (ignore char))
  (when arg
    (error "Unexpected argument for set literal: ~A" arg))
  (let ((forms (read-delimited-list #\} stream t)))
    `(make-set :items (list ,@forms))))

(defun syntax-error (stream char)
  (declare (ignore stream))
  (error "Unmatched ~A" char))

(defreadtable syntax
  (:merge :standard)
  (:macro-char #\[ #'read-vector nil)
  (:macro-char #\{ #'read-map nil)
  (:dispatch-macro-char #\# #\{ #'read-set)
  (:macro-char #\] #'syntax-error nil)
  (:macro-char #\} #'syntax-error nil))
