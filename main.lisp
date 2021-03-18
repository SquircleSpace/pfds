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
  (:use :pfds.shcl.io/utility/interface)
  (:use-reexport :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/implementation/weight-balanced-tree
   #:<weight-balanced-map>
   #:<weight-balanced-set>
   #:<weight-balanced-sequence>)
  (:import-from :pfds.shcl.io/implementation/leftist-heap
   #:<leftist-heap>)
  (:import-from :pfds.shcl.io/implementation/persistent-vector
   #:<persistent-vector>)
  (:import-from :pfds.shcl.io/implementation/bankers-queue
   #:<bankers-queue>)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare)
  (:import-from :named-readtables
   #:defreadtable)
  (:export
   #:make-map
   #:make-set
   #:make-seq
   #:make-priority-queue
   #:make-queue
   #:make-stack
   #:syntax
   #:compare))
(in-package :pfds.shcl.io)

(declaim (inline make-map))
(defun make-map (&key plist alist (comparator #'compare))
  "Make an ordered map.

The map will contain the associations described by the given alist and
plist.  If both the alist and the plist contain a key, then it is
unspecified which value will be contained in the map."
  (i-make-ordered-map <weight-balanced-map> :comparator comparator :plist plist :alist alist))

(declaim (inline make-set))
(defun make-set (&key items (comparator #'compare))
  "Make an ordered set."
  (i-make-ordered-set <weight-balanced-set> :comparator comparator :items items))

(declaim (inline make-priority-queue))
(defun make-priority-queue (&key items (comparator #'compare))
  "Make a priority queue."
  (i-make-priority-queue <leftist-heap> :comparator comparator :items items))

(declaim (inline make-queue))
(defun make-queue (&key items)
  "Make a queue."
  (i-make-queue <bankers-queue> :items items))

(declaim (inline make-stack))
(defun make-stack (&key items)
  "Make a stack."
  ;; Sssshh.  Don't tell anyone.
  (copy-list items))

(declaim (inline make-seq))
(defun make-seq (&key items)
  "Make a general-purpose sequence."
  (i-make-seq <weight-balanced-sequence> :items items))

(defun read-seq (stream char)
  (declare (ignore char))
  (let ((forms (read-delimited-list #\] stream t)))
    `(make-seq :items (list ,@forms))))

(defun read-map (stream)
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

(defun read-set (stream)
  (let ((forms (read-delimited-list #\} stream t)))
    `(make-set :items (list ,@forms))))

(defun read-map-or-set (stream char)
  (declare (ignore char))
  (let ((next-char (peek-char nil stream t :eof t)))
    (cond
      ((equal next-char #\{)
       (read-char stream t :eof t)
       (prog1
           (read-set stream)
         (let ((last-char (read-char stream t :eof t)))
           (unless (equal last-char #\})
             (error "Only one closing } for set!  Instead, got ~A" last-char)))))

      (t
       (read-map stream)))))

(defun syntax-error (stream char)
  (declare (ignore stream))
  (error "Unmatched ~A" char))

(defreadtable syntax
  (:merge :standard)
  (:macro-char #\[ #'read-seq nil)
  (:macro-char #\{ #'read-map-or-set nil)
  (:macro-char #\] #'syntax-error nil)
  (:macro-char #\} #'syntax-error nil))
