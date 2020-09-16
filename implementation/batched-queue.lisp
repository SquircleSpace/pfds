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
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:iterator-flatten* #:compare-containers)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:export
   #:batched-queue
   #:batched-queue-p
   #:make-batched-queue))
(in-package :pfds.shcl.io/implementation/batched-queue)

;; See "Purely Functional Data Structures" by Chris Okasaki

(define-immutable-structure (batched-queue (:constructor %make-batched-queue))
  (front-stack nil :type list)
  (back-stack nil :type list)
  (count 0 :type (integer 0)))

(declare-interface-conformance batched-queue queue)

(defmethod check-invariants ((queue batched-queue))
  (with-accessors ((front batched-queue-front-stack)
                   (back batched-queue-back-stack)
                   (count batched-queue-count))
      queue
    (cassert (or (null back)
                 front)
             nil "back stack cannot be non-nil unless front is non-nil")
    (cassert (equal count
                    (+ (length front)
                       (length back)))
             nil "Count must be accurate")))

(defmethod print-graphviz ((queue batched-queue) stream id-vendor)
  (let ((id (next-graphviz-id id-vendor))
        last-id)
    (labels
        ((link (this-id)
           (when last-id
             (format stream "ID~A -> ID~A~%" last-id this-id))
           (setf last-id this-id))

         (link-list (list)
           (do-each (value list)
             (let ((node-id (next-graphviz-id id-vendor)))
               (format stream "ID~A [label=\"~A\"]~%" node-id value)
               (link node-id)))))

      (format stream "ID~A [label=\"(middle)\" color=gray]~%" id)
      (link-list (batched-queue-front-stack queue))
      (link id)
      (link-list (reverse (batched-queue-front-stack queue)))
      id)))

(defmethod to-list ((queue batched-queue))
  (append (batched-queue-front-stack queue)
          (reverse (batched-queue-back-stack queue))))

(defmethod iterator ((queue batched-queue))
  (iterator-flatten* (iterator (batched-queue-front-stack queue))
                     (iterator (reverse (batched-queue-back-stack queue)))))

(defmethod for-each ((queue batched-queue) function)
  (for-each (batched-queue-front-stack queue) function)
  (for-each (reverse (batched-queue-back-stack queue)) function))

(defmethod map-members ((queue batched-queue) function)
  (cond
    ((null (batched-queue-back-stack queue))
     (copy-batched-queue queue :front-stack (map-members (batched-queue-front-stack queue) function)))

    (t
     (let ((builder (make-impure-list-builder)))
       (for-each queue (lambda (value)
                         (impure-list-builder-add builder (funcall function value))))
       (copy-batched-queue queue
                          :front-stack (impure-list-builder-extract builder)
                          :count (batched-queue-count queue)
                          :back-stack nil)))))

(defmethod print-object ((queue batched-queue) stream)
  (if *print-readably*
      (call-next-method)
      (print-container queue stream)))

(defvar *empty-batched-queue* (%make-batched-queue))

(defun make-batched-queue (&key items)
  (if items
      (%make-batched-queue
       :front-stack (copy-list items)
       :count (length items))
      *empty-batched-queue*))

(defun batched-queue (&rest items)
  (make-batched-queue :items items))

(defun batched-queue-with-back (queue item)
  (if (batched-queue-front-stack queue)
      (copy-batched-queue
       queue
       :back-stack (cons item (batched-queue-back-stack queue))
       :count (1+ (batched-queue-count queue)))
      (copy-batched-queue
       queue
       :front-stack (cons item nil)
       :count (1+ (batched-queue-count queue)))))

(defmethod with-back ((queue batched-queue) item)
  (batched-queue-with-back queue item))

(defun batched-queue-without-front (queue)
  (let ((front-stack (batched-queue-front-stack queue)))
    (unless front-stack
      (return-from batched-queue-without-front
        (values queue nil nil)))

    (let ((new-front-stack (cdr front-stack))
          (value (car front-stack))
          (back-stack (batched-queue-back-stack queue)))
      (values
       (cond
         (new-front-stack
          (copy-batched-queue queue :front-stack new-front-stack
                                    :count (1- (batched-queue-count queue))))
         (back-stack
          (copy-batched-queue queue :front-stack (reverse back-stack)
                                    :back-stack nil
                                    :count (1- (batched-queue-count queue))))
         (t
          *empty-batched-queue*))
       value
       t))))

(defmethod without-front ((queue batched-queue))
  (batched-queue-without-front queue))

(defmethod peek-front ((queue batched-queue))
  (if (batched-queue-front-stack queue)
      (values (car (batched-queue-front-stack queue))
              t)
      (values nil nil)))

(defmethod is-empty ((queue batched-queue))
  (null (batched-queue-front-stack queue)))

(defmethod empty ((queue batched-queue))
  *empty-batched-queue*)

(defmethod size ((queue batched-queue))
  (batched-queue-count queue))

(defmethod compare-objects ((left batched-queue) (right batched-queue))
  (compare-containers left right #'compare))

(defmethod with-member ((queue batched-queue) object)
  (batched-queue-with-back queue object))

(defmethod decompose ((queue batched-queue))
  (batched-queue-without-front queue))
