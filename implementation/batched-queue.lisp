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

(defpackage :pfds.shcl.io/implementation/batched-queue
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:check-invariants)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/interface/queue
   #:with-last #:without-first #:peek-first #:is-empty #:empty)
  (:export
   #:with-last
   #:without-first
   #:peek-first
   #:is-empty
   #:empty
   #:batched-queue
   #:batched-queue-p
   #:make-batched-queue))
(in-package :pfds.shcl.io/implementation/batched-queue)

;; See "Purely Functional Data Structures" by Chris Okasaki

(define-immutable-structure (batched-queue (:constructor %make-batched-queue))
  (front-stack nil :type list)
  (back-stack nil :type list))

(defmethod check-invariants ((queue batched-queue))
  (with-accessors ((front batched-queue-front-stack)
                   (back batched-queue-back-stack))
      queue
    (cassert (or (null back)
                 front)
             nil "back stack cannot be non-nil unless front is non-nil")))

(defmethod to-list ((queue batched-queue))
  (append (batched-queue-front-stack queue) (reverse (batched-queue-back-stack queue))))

(defmethod print-object ((queue batched-queue) stream)
  (write
   (if *print-readably*
       `(make-batched-queue :items ',(to-list queue))
       `(batched-queue ,@(to-list queue)))
   :stream stream))

(defvar *empty-batched-queue* (%make-batched-queue))

(defun make-batched-queue (&key items)
  (if items
      (%make-batched-queue
       :front-stack (copy-list items))
      *empty-batched-queue*))

(defun batched-queue (&rest items)
  (make-batched-queue :items items))

(defmethod with-last ((queue batched-queue) item)
  (if (batched-queue-front-stack queue)
      (copy-batched-queue
       queue
       :back-stack (cons item (batched-queue-back-stack queue)))
      (copy-batched-queue
       queue
       :front-stack (cons item nil))))

(defmethod without-first ((queue batched-queue))
  (let ((front-stack (batched-queue-front-stack queue)))
    (unless front-stack
      (return-from without-first
        (values queue nil nil)))

    (let ((new-front-stack (cdr front-stack))
          (value (car front-stack))
          (back-stack (batched-queue-back-stack queue)))
      (values
       (cond
         (new-front-stack
          (copy-batched-queue queue :front-stack new-front-stack))
         (back-stack
          (copy-batched-queue queue :front-stack (reverse back-stack) :back-stack nil))
         (t
          *empty-batched-queue*))
       value
       t))))

(defmethod peek-first ((queue batched-queue))
  (if (batched-queue-front-stack queue)
      (values (car (batched-queue-front-stack queue))
              t)
      (values nil nil)))

(defmethod is-empty ((queue batched-queue))
  (null (batched-queue-front-stack queue)))

(defmethod empty ((queue batched-queue))
  *empty-batched-queue*)
