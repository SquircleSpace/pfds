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

(defpackage :pfds.shcl.io/implementation/bankers-queue
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/utility/lazy
   #:force #:lazy)
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:check-invariants)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:import-from :pfds.shcl.io/implementation/lazy-list
   #:empty-lazy-list #:lazy-list-append
   #:lazy-list-reverse #:head #:tail #:lazy-cons
   #:lazy-list-length)
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
   #:bankers-queue
   #:bankers-queue-p
   #:make-bankers-queue
   #:make-bankers-queue*))
(in-package :pfds.shcl.io/implementation/bankers-queue)

;; See "Purely Functional Data Structures" by Chris Okasaki

(define-immutable-structure (bankers-queue (:constructor %make-bankers-queue))
  (front-stack (empty-lazy-list))
  (front-stack-size 0 :type (integer 0))
  (back-stack (empty-lazy-list))
  (back-stack-size 0 :type (integer 0)))

(defvar *empty-bankers-queue*
  (%make-bankers-queue))

(defun balance-bankers-queue (front-stack front-stack-size back-stack back-stack-size)
  (assert (and (not (minusp front-stack-size))
               (not (minusp back-stack-size))))

  (when (> back-stack-size front-stack-size)
    (setf front-stack (lazy-list-append front-stack (lazy-list-reverse back-stack)))
    ;; force the first element of the new front stack so its always
    ;; ready to go
    (head front-stack)
    (setf front-stack-size (+ front-stack-size back-stack-size))
    (setf back-stack (empty-lazy-list))
    (setf back-stack-size 0))

  (when (zerop front-stack-size)
    (return-from balance-bankers-queue
      *empty-bankers-queue*))

  (%make-bankers-queue :front-stack front-stack
                       :front-stack-size front-stack-size
                       :back-stack back-stack
                       :back-stack-size back-stack-size))

(defmethod with-last ((queue bankers-queue) item)
  (balance-bankers-queue
   (bankers-queue-front-stack queue)
   (bankers-queue-front-stack-size queue)
   (lazy-cons item (bankers-queue-back-stack queue))
   (1+ (bankers-queue-back-stack-size queue))))

(defmethod without-first ((queue bankers-queue))
  (when (zerop (bankers-queue-front-stack-size queue))
    (return-from without-first
      (values queue nil nil)))

  (multiple-value-bind
        (new-front-stack head-value valid-p)
      (tail (bankers-queue-front-stack queue))
    (assert valid-p nil "If the front stack is non-empty, it should report as such when calling tail")
    (values
     (balance-bankers-queue
      new-front-stack
      (1- (bankers-queue-front-stack-size queue))
      (bankers-queue-back-stack queue)
      (bankers-queue-back-stack-size queue))
     head-value
     valid-p)))

(defmethod peek-first ((queue bankers-queue))
  (head (bankers-queue-front-stack queue)))

(defmethod is-empty ((queue bankers-queue))
  (zerop (bankers-queue-front-stack-size queue)))

(defmethod empty ((queue bankers-queue))
  *empty-bankers-queue*)

(defun reverse-and-lazy (list)
  (let ((result (empty-lazy-list))
        (count 0))
    (dolist (value list)
      (setf result (lazy-cons value result))
      (incf count))
    (values result count)))

(defun make-bankers-queue* (&key items)
  (if items
      (multiple-value-bind (back-stack back-stack-size) (reverse-and-lazy items)
        (balance-bankers-queue (empty-lazy-list) 0 back-stack back-stack-size))
      *empty-bankers-queue*))

(defun make-bankers-queue (&rest items)
  (make-bankers-queue* :items items))

(defmethod to-list ((queue bankers-queue))
  (to-list (lazy-list-append (bankers-queue-front-stack queue)
                             (lazy-list-reverse (bankers-queue-back-stack queue)))))

(defmethod print-object ((queue bankers-queue) stream)
  (write `(make-bankers-queue* :items ',(to-list queue))
         :stream stream))

(defmethod check-invariants ((queue bankers-queue))
  (cassert (>= (bankers-queue-front-stack-size queue)
               (bankers-queue-back-stack-size queue)))
  (cassert (equal (bankers-queue-front-stack-size queue)
                  (lazy-list-length (bankers-queue-front-stack queue))))
  (cassert (equal (bankers-queue-back-stack-size queue)
                  (lazy-list-length (bankers-queue-back-stack queue)))))
