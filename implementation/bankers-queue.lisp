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
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/lazy
   #:force #:lazy)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-containers #:iterator-flatten*)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:import-from :pfds.shcl.io/implementation/lazy-list
   #:lazy-list-append
   #:lazy-list-reverse #:head #:tail #:lazy-cons
   #:lazy-list-length #:lazy-list #:make-lazy-list)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:export
   #:bankers-queue
   #:bankers-queue-p
   #:make-bankers-queue))
(in-package :pfds.shcl.io/implementation/bankers-queue)

;; See "Purely Functional Data Structures" by Chris Okasaki

(define-immutable-structure (bankers-queue (:constructor %make-bankers-queue))
  (front-stack (lazy-list))
  (front-stack-size 0 :type (integer 0))
  (back-stack (lazy-list))
  (back-stack-size 0 :type (integer 0)))

(declare-interface-conformance bankers-queue queue)

(defvar *empty-bankers-queue*
  (%make-bankers-queue))

(defun balance-bankers-queue (front-stack front-stack-size back-stack back-stack-size)
  (assert (and (not (minusp front-stack-size))
               (not (minusp back-stack-size))))

  (when (> back-stack-size front-stack-size)
    (setf front-stack (lazy-list-append front-stack (lazy-list-reverse back-stack)))
    ;; force the first element of the new front stack so its always
    ;; ready to go
    (peek-top front-stack)
    (setf front-stack-size (+ front-stack-size back-stack-size))
    (setf back-stack (lazy-list))
    (setf back-stack-size 0))

  (when (zerop front-stack-size)
    (return-from balance-bankers-queue
      *empty-bankers-queue*))

  (%make-bankers-queue :front-stack front-stack
                       :front-stack-size front-stack-size
                       :back-stack back-stack
                       :back-stack-size back-stack-size))

(defun bankers-queue-with-back (queue item)
  (balance-bankers-queue
   (bankers-queue-front-stack queue)
   (bankers-queue-front-stack-size queue)
   (lazy-cons item (bankers-queue-back-stack queue))
   (1+ (bankers-queue-back-stack-size queue))))

(defmethod with-back ((queue bankers-queue) item)
  (bankers-queue-with-back queue item))

(defun bankers-queue-without-front (queue)
  (when (zerop (bankers-queue-front-stack-size queue))
    (return-from bankers-queue-without-front
      (values queue nil nil)))

  (multiple-value-bind
        (new-front-stack head-value valid-p)
      (without-top (bankers-queue-front-stack queue))
    (assert valid-p nil "If the front stack is non-empty, it should report as such when calling tail")
    (values
     (balance-bankers-queue
      new-front-stack
      (1- (bankers-queue-front-stack-size queue))
      (bankers-queue-back-stack queue)
      (bankers-queue-back-stack-size queue))
     head-value
     valid-p)))

(defmethod without-front ((queue bankers-queue))
  (bankers-queue-without-front queue))

(defmethod peek-front ((queue bankers-queue))
  (peek-top (bankers-queue-front-stack queue)))

(defmethod is-empty ((queue bankers-queue))
  (zerop (bankers-queue-front-stack-size queue)))

(defmethod empty ((queue bankers-queue))
  *empty-bankers-queue*)

(defun make-bankers-queue (&key items)
  (if items
      (balance-bankers-queue (make-lazy-list :items items) (length items) (lazy-list) 0)
      *empty-bankers-queue*))

(defun bankers-queue (&rest items)
  (make-bankers-queue :items items))

(defmethod to-list ((queue bankers-queue))
  (nconc (to-list (bankers-queue-front-stack queue))
         (nreverse (to-list (bankers-queue-back-stack queue)))))

(defmethod for-each ((queue bankers-queue) function)
  (for-each (bankers-queue-front-stack queue) function)
  (for-each (nreverse (to-list (bankers-queue-back-stack queue))) function))

(defmethod map-members ((queue bankers-queue) function)
  (let ((size (+ (bankers-queue-front-stack-size queue)
                 (bankers-queue-back-stack-size queue))))
    (when (zerop size)
      (return-from map-members queue))

    (let ((builder (make-impure-list-builder))
          (return-input t))
      (for-each queue (lambda (v)
                        (let ((new-item (funcall function v)))
                          (impure-list-builder-add builder new-item)
                          (unless (eql new-item v)
                            (setf return-input nil)))))
      (if return-input
          queue
          (balance-bankers-queue (make-lazy-list :items (impure-list-builder-extract builder)) size
                                 (lazy-list) 0)))))

(defmethod iterator ((queue bankers-queue))
  (let ((front-iterator (iterator (bankers-queue-front-stack queue)))
        (back-iterator (iterator (lazy-list-reverse (bankers-queue-back-stack queue)))))
    (iterator-flatten* front-iterator back-iterator)))

(defmethod print-object ((queue bankers-queue) stream)
  (if *print-readably*
      (call-next-method)
      (print-container queue stream)))

(defmethod check-invariants ((queue bankers-queue))
  (cassert (>= (bankers-queue-front-stack-size queue)
               (bankers-queue-back-stack-size queue)))
  (cassert (equal (bankers-queue-front-stack-size queue)
                  (lazy-list-length (bankers-queue-front-stack queue))))
  (cassert (equal (bankers-queue-back-stack-size queue)
                  (lazy-list-length (bankers-queue-back-stack queue)))))

(defmethod print-graphviz ((queue bankers-queue) stream id-vendor)
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
      (link-list (bankers-queue-front-stack queue))
      (link id)
      (link-list (lazy-list-reverse (bankers-queue-back-stack queue)))
      id)))

(defmethod size ((queue bankers-queue))
  (+ (bankers-queue-front-stack-size queue)
     (bankers-queue-back-stack-size queue)))

(defmethod compare-objects ((left bankers-queue) (right bankers-queue))
  (compare-containers left right #'compare))

(defmethod with-member ((queue bankers-queue) object)
  (bankers-queue-with-back queue object))

(defmethod decompose ((queue bankers-queue))
  (bankers-queue-without-front queue))
