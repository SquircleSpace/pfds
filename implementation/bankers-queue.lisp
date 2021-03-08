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

(uiop:define-package :pfds.shcl.io/implementation/bankers-queue
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/utility/specialization
   #:named-specialize*)
  (:import-from :pfds.shcl.io/utility/lazy
   #:force #:lazy)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-collection-contents #:iterator-flatten*)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:import-from :pfds.shcl.io/implementation/lazy-list
   #:<lazy-list> #:lazy-list)
  (:import-from :pfds.shcl.io/implementation/list
   #:<list>)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:export
   #:<bankers-queue>
   #:bankers-queue
   #:bankers-queue-p))
(in-package :pfds.shcl.io/implementation/bankers-queue)

;; See "Purely Functional Data Structures" by Chris Okasaki

(define-immutable-structure (bankers-queue (:constructor %make-bankers-queue))
  (front-stack (lazy-list) :type lazy-list)
  (front-stack-size 0 :type (integer 0))
  (back-stack (lazy-list) :type lazy-list)
  (back-stack-size 0 :type (integer 0)))

(define-simple-interface-instance <bankers-queue> <<queue>> bankers-queue-
  'make-queue 'make-bankers-queue)

(defvar *empty-bankers-queue*
  (%make-bankers-queue))

(defun balance-bankers-queue (front-stack front-stack-size back-stack back-stack-size)
  (assert (and (not (minusp front-stack-size))
               (not (minusp back-stack-size))))

  (when (> back-stack-size front-stack-size)
    (setf front-stack (i-join <lazy-list> front-stack (i-reverse <lazy-list> back-stack)))
    ;; force the first element of the new front stack so its always
    ;; ready to go
    (i-peek-top <lazy-list> front-stack)
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

(declaim (inline bankers-queue-with-back))
(defun bankers-queue-with-back (queue item)
  (balance-bankers-queue
   (bankers-queue-front-stack queue)
   (bankers-queue-front-stack-size queue)
   (i-with-top <lazy-list> (bankers-queue-back-stack queue) item)
   (1+ (bankers-queue-back-stack-size queue))))

(defun bankers-queue-without-front (queue)
  (when (zerop (bankers-queue-front-stack-size queue))
    (return-from bankers-queue-without-front
      (values queue nil nil)))

  (multiple-value-bind
        (new-front-stack head-value valid-p)
      (i-without-top <lazy-list> (bankers-queue-front-stack queue))
    (assert valid-p nil "If the front stack is non-empty, it should report as such when calling tail")
    (values
     (balance-bankers-queue
      new-front-stack
      (1- (bankers-queue-front-stack-size queue))
      (bankers-queue-back-stack queue)
      (bankers-queue-back-stack-size queue))
     head-value
     valid-p)))

(declaim (inline bankers-queue-peek-front))
(defun bankers-queue-peek-front (queue)
  (i-peek-top <lazy-list> (bankers-queue-front-stack queue)))

(declaim (inline bankers-queue-is-empty))
(defun bankers-queue-is-empty (queue)
  (zerop (bankers-queue-front-stack-size queue)))

(declaim (inline bankes-queue-empty))
(defun bankers-queue-empty (queue)
  (declare (ignore queue))
  *empty-bankers-queue*)

(defun bankers-queue-representative-empty ()
  *empty-bankers-queue*)

(declaim (inline make-bankers-queue))
(defun make-bankers-queue (&key items)
  (if items
      (balance-bankers-queue (i-make-stack <lazy-list> :items items) (length items) (lazy-list) 0)
      *empty-bankers-queue*))

(declaim (inline bankers-queue))
(defun bankers-queue (&rest items)
  (make-bankers-queue :items items))

(declaim (inline bankers-queue-for-each))
(defun bankers-queue-for-each (queue function)
  (i-for-each <lazy-list> (bankers-queue-front-stack queue) function)
  (i-for-each <list> (nreverse (i-to-list <lazy-list> (bankers-queue-back-stack queue))) function))

(named-specialize*
  (bankers-queue-to-list (collection-to-list <bankers-queue>))
  (bankers-queue-compare (compare-collection-contents <bankers-queue>)))

(declaim (inline bankers-queue-to-list))
(defun bankers-queue-to-list (queue)
  (collection-to-list <bankers-queue> queue))

(declaim (inline bankers-queue-size))
(defun bankers-queue-size (queue)
  (+ (bankers-queue-front-stack-size queue)
     (bankers-queue-back-stack-size queue)))

(defun bankers-queue-map-members (queue function)
  (let ((size (bankers-queue-size queue)))
    (when (zerop size)
      (return-from bankers-queue-map-members queue))

    (let ((builder (make-impure-list-builder))
          (return-input t))
      (bankers-queue-for-each
       queue
       (lambda (v)
         (let ((new-item (funcall function v)))
           (impure-list-builder-add builder new-item)
           (unless (eql new-item v)
             (setf return-input nil)))))
      (if return-input
          queue
          (balance-bankers-queue (i-make-stack <lazy-list> :items (impure-list-builder-extract builder)) size
                                 (lazy-list) 0)))))

(declaim (inline bankers-queue-iterator))
(defun bankers-queue-iterator (queue)
  (let ((front-iterator (i-iterator <lazy-list> (bankers-queue-front-stack queue)))
        (back-iterator (i-iterator <lazy-list> (i-reverse <lazy-list> (bankers-queue-back-stack queue)))))
    (iterator-flatten* front-iterator back-iterator)))

(defmethod print-object ((queue bankers-queue) stream)
  (if *print-readably*
      (call-next-method)
      (print-container <bankers-queue> queue stream)))

(defun bankers-queue-check-invariants (queue)
  (cassert (>= (bankers-queue-front-stack-size queue)
               (bankers-queue-back-stack-size queue)))
  (cassert (equal (bankers-queue-front-stack-size queue)
                  (i-size <lazy-list> (bankers-queue-front-stack queue))))
  (cassert (equal (bankers-queue-back-stack-size queue)
                  (i-size <lazy-list> (bankers-queue-back-stack queue)))))

(defun bankers-queue-print-graphviz (queue stream id-vendor)
  (let ((id (next-graphviz-id id-vendor))
        last-id)
    (labels
        ((link (this-id)
           (when last-id
             (format stream "ID~A -> ID~A~%" last-id this-id))
           (setf last-id this-id))

         (link-list (list)
           (i-for-each <lazy-list> list
                       (lambda (value)
                         (let ((node-id (next-graphviz-id id-vendor)))
                           (format stream "ID~A [label=\"~A\"]~%" node-id value)
                           (link node-id))))))

      (format stream "ID~A [label=\"(middle)\" color=gray]~%" id)
      (link-list (bankers-queue-front-stack queue))
      (link id)
      (link-list (i-reverse <lazy-list> (bankers-queue-back-stack queue)))
      id)))

(declaim (inline bankers-queue-with-member))
(defun bankers-queue-with-member (queue object)
  (bankers-queue-with-back queue object))

(declaim (inline bankers-queue-decompose))
(defun bankers-queue-decompose (queue)
  (bankers-queue-without-front queue))

(define-interface-methods <bankers-queue> bankers-queue)
