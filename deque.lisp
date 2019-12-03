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

(defpackage :pfds.shcl.io/deque
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common
   #:define-interface)
  (:import-from :pfds.shcl.io/queue
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
(in-package :pfds.shcl.io/deque)

(define-interface deque
  is-empty
  empty
  with-last
  (defgeneric with-first (queue item))
  without-first
  (defgeneric without-last (queue))
  peek-first
  (defgeneric peek-last (queue)))

(defmethod peek-last (queue)
  (multiple-value-bind (new-queue value valid-p) (without-last queue)
    (declare (ignore new-queue))
    (values value valid-p)))
