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

(defpackage :pfds.shcl.io/interface/queue
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:is-empty #:empty #:define-interface)
  (:export
   #:with-last
   #:without-first
   #:peek-first
   #:is-empty
   #:empty))
(in-package :pfds.shcl.io/interface/queue)

(define-interface queue
  is-empty
  empty

  (defgeneric with-last (queue object)
    (:documentation
     "Return a queue with an object added to the end."))

  (defgeneric without-first (queue)
    (:documentation
     "Return a queue where the first element has been removed.

If the queue is empty, this returns an empty queue and two nil values.
If the queue is non-empty, it returns the updated queue, the removed
value, and a non-nil value."))

  (defgeneric peek-first (queue)
    (:documentation
     "Retrieve the object at the front of the queue.

If the queue is empty, this returns two nil values.  If the queue is
non-empty, it returns the first object in the queue and a non-nil
value.")))

(defmethod peek-first (queue)
  (multiple-value-bind (new-queue value valid-p) (without-first queue)
    (declare (ignore new-queue))
    (values value valid-p)))
