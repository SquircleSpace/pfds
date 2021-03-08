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

(uiop:define-package :pfds.shcl.io/implementation/batched-queue
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/utility/specialization
   #:named-specialize*)
  (:import-from :pfds.shcl.io/implementation/list
   #:<list>)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:iterator-flatten* #:compare-collection-contents)
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
   #:<batched-queue>
   #:batched-queue
   #:batched-queue-p))
(in-package :pfds.shcl.io/implementation/batched-queue)

;; See "Purely Functional Data Structures" by Chris Okasaki

(define-immutable-structure (batched-queue (:constructor %make-batched-queue))
  (front-stack nil :type list)
  (back-stack nil :type list)
  (count 0 :type (integer 0)))

(define-simple-interface-instance <batched-queue> <<queue>> batched-queue-
  'make-queue 'make-batched-queue)

(defun batched-queue-check-invariants (queue)
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

(defun batched-queue-print-graphviz (queue stream id-vendor)
  (let ((id (next-graphviz-id id-vendor))
        last-id)
    (labels
        ((link (this-id)
           (when last-id
             (format stream "ID~A -> ID~A~%" last-id this-id))
           (setf last-id this-id))

         (link-list (list)
           (dolist (value list)
             (let ((node-id (next-graphviz-id id-vendor)))
               (format stream "ID~A [label=\"~A\"]~%" node-id value)
               (link node-id)))))

      (format stream "ID~A [label=\"(middle)\" color=gray]~%" id)
      (link-list (batched-queue-front-stack queue))
      (link id)
      (link-list (reverse (batched-queue-front-stack queue)))
      id)))

(declaim (inline batched-queue-to-list))
(defun batched-queue-to-list (queue)
  (append (batched-queue-front-stack queue)
          (reverse (batched-queue-back-stack queue))))

(declaim (inline batched-queue-iterator))
(defun batched-queue-iterator (queue)
  (iterator-flatten*
   (i-iterator <list> (batched-queue-front-stack queue))
   (i-iterator <list> (reverse (batched-queue-back-stack queue)))))

(declaim (inline batched-queue-for-each))
(defun batched-queue-for-each (queue function)
  (i-for-each <list> (batched-queue-front-stack queue) function)
  (i-for-each <list> (reverse (batched-queue-back-stack queue)) function))

(defun batched-queue-map-members (queue function)
  (cond
    ((null (batched-queue-back-stack queue))
     (copy-batched-queue
      queue
      :front-stack (i-map-members <list> (batched-queue-front-stack queue) function)))

    (t
     (let ((builder (make-impure-list-builder)))
       (i-for-each
        <batched-queue>
        queue
        (lambda (value)
          (impure-list-builder-add builder (funcall function value))))

       (copy-batched-queue queue
                          :front-stack (impure-list-builder-extract builder)
                          :count (batched-queue-count queue)
                          :back-stack nil)))))

(defmethod print-object ((queue batched-queue) stream)
  (if *print-readably*
      (call-next-method)
      (print-container <batched-queue> queue stream)))

(defvar *empty-batched-queue* (%make-batched-queue))

(declaim (inline make-batched-queue))
(defun make-batched-queue (&key items)
  (if items
      (%make-batched-queue
       :front-stack (copy-list items)
       :count (length items))
      *empty-batched-queue*))

(declaim (inline batched-queue))
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

(declaim (inline batched-queue-peek-front))
(defun batched-queue-peek-front (queue)
  (if (batched-queue-front-stack queue)
      (values (car (batched-queue-front-stack queue))
              t)
      (values nil nil)))

(declaim (inline batched-queue-is-empty))
(defun batched-queue-is-empty (queue)
  (null (batched-queue-front-stack queue)))

(declaim (inline batched-queue-empty))
(defun batched-queue-empty (queue)
  (declare (ignore queue))
  *empty-batched-queue*)

(declaim (inline batched-queue-representative-empty))
(defun batched-queue-representative-empty ()
  *empty-batched-queue*)

(declaim (inline batched-queue-size))
(defun batched-queue-size (queue)
  (batched-queue-count queue))

(named-specialize*
  (batched-queue-compare (compare-collection-contents <batched-queue>)))

(declaim (inline batched-queue-with-member))
(defun batched-queue-with-member (queue object)
  (batched-queue-with-back queue object))

(declaim (inline batched-queue-decompose))
(defun batched-queue-decompose (queue)
  (batched-queue-without-front queue))

(define-interface-methods <batched-queue> batched-queue)
