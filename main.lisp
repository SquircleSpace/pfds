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
   #:make-red-black-map*
   #:make-red-black-set*)
  (:import-from :pfds.shcl.io/implementation/leftist-heap
   #:make-leftist-heap*)
  (:import-from :pfds.shcl.io/compare
   #:compare)
  (:export
   #:make-map #:make-map*
   #:make-set #:make-set*
   #:make-min-heap #:make-min-heap* #:make-heap
   #:compare))
(in-package :pfds.shcl.io)

(declaim (inline make-map))
(defun make-map (&rest plist)
  "Make an ordered map.

The map will contain the associations described by the given plist."
  (make-red-black-map* #'compare :plist plist))

(declaim (inline make-map*))
(defun make-map* (&key plist alist (comparator #'compare))
  "Make an ordered map.

The map will contain the associations described by the given alist and
plist.  If both the alist and the plist contain a key, then it is
unspecified which value will be contained in the map."
  (make-red-black-map* comparator :plist plist :alist alist))

(declaim (inline make-set))
(defun make-set (&rest items)
  "Make an ordered set."
  (make-red-black-set* #'compare :items items))

(declaim (inline make-set*))
(defun make-set* (&key items (comparator #'compare))
  "Make an ordered set."
  (make-red-black-set* comparator :items items))

(declaim (inline make-min-heap))
(defun make-min-heap (&rest items)
  "Make a min heap."
  (make-leftist-heap* #'compare :items items))

(declaim (inline make-max-heap))
(defun make-max-heap (&rest items)
  "Make a max heap."
  (make-leftist-heap* (lambda (l r) (compare r l)) :items items))

(declaim (inline make-heap*))
(defun make-heap* (&key (priority :min) items (comparator #'compare))
  "Make a heap."
  (check-type priority (member :min :max))
  (make-leftist-heap* (if (eq priority :max)
                          (lambda (l r) (funcall comparator r l))
                          comparator)
                      :items items))
