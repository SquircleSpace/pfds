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

(defpackage :pfds.shcl.io/interface/deque
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:define-interface)
  (:import-from :pfds.shcl.io/interface/queue
   #:with-last
   #:without-first
   #:peek-first
   #:is-empty
   #:empty)
  (:export
   #:with-last
   #:with-first
   #:without-first
   #:without-last
   #:peek-first
   #:peek-last
   #:is-empty
   #:empty))
(in-package :pfds.shcl.io/interface/deque)

(define-interface deque
  is-empty
  empty

  with-last

  (defgeneric with-first (deque object)
    (:documentation
     "Return a new deque with `OBJECT' at the front."))

  without-first

  (defgeneric without-last (deque)
    (:documentation
     "Return a new deque with the object at the end removed.

If the deque is empty, this returns an empty deque and two nil values.
If the deque is non-empty, this returns the new deque, the object
removed from the deque, and nil."))

  peek-first

  (defgeneric peek-last (deque)
    (:documentation
     "Return the object at the end of the deque.

If the deque is empty, this returns two nil values.  If the deque is
non-empty, it returns the final element and a non-nil value.")))

(defmethod peek-last (queue)
  (multiple-value-bind (new-queue value valid-p) (without-last queue)
    (declare (ignore new-queue))
    (values value valid-p)))
