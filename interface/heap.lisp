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

(defpackage :pfds.shcl.io/interface/heap
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:is-empty #:empty #:with-member #:define-interface)
  (:export
   #:heap
   #:merge-heaps
   #:heap-top
   #:without-heap-top
   #:with-member
   #:is-empty
   #:empty))
(in-package :pfds.shcl.io/interface/heap)

(define-interface heap
  (defgeneric merge-heaps (first second)
    (:documentation
     "Return a new heap that contains all the elements in the given heaps."))

  (defgeneric heap-top (heap)
    (:documentation
     "Return the element at the top of the heap.

If the heap is empty, this returns two nil values.  If the heap is
non-empty, it returns the smallest element and a non-nil value."))

  (defgeneric without-heap-top (heap)
    (:documentation
     "Return a heap where the top element has been removed.

If the heap is empty, this returns an empty heap and two nil values.
If the heap is non-empty, it returns the updated heap, the element
removed, and a non-nil value."))

  with-member

  is-empty
  empty)
