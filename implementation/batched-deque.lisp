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

(uiop:define-package :pfds.shcl.io/implementation/batched-deque
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/utility/specialization
   #:named-specialize*)
  (:import-from :pfds.shcl.io/implementation/list
   #:<list>)
  (:import-from :pfds.shcl.io/utility/list
   #:impure-destructive-mapcar)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-collection-contents #:iterator-flatten*)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:export
   #:<batched-deque>
   #:batched-deque
   #:batched-deque-p))
(in-package :pfds.shcl.io/implementation/batched-deque)

;; See "Purely Functional Data Structures" by Chris Okasaki

(define-immutable-structure (batched-deque (:constructor %make-batched-deque))
  (front-stack nil :type list)
  (front-count 0 :type (integer 0))
  (back-stack nil :type list)
  (back-count 0 :type (integer 0)))

(define-simple-interface-instance <batched-deque> <<deque>> batched-deque-
  'make-queue 'make-batched-deque
  'make-deque 'make-batched-deque)

(defun batched-deque-check-invariants (deque)
  (with-accessors ((front batched-deque-front-stack)
                   (front-count batched-deque-front-count)
                   (back batched-deque-back-stack)
                   (back-count batched-deque-back-count))
      deque
    (cassert (or (null back)
                 front)
             nil "back stack cannot be non-nil unless front is non-nil")
    (cassert (equal front-count (length front))
             nil "front count has the correct value")
    (cassert (equal back-count (length back))
             nil "back count has the correct value")))

(defun batched-deque-print-graphviz (deque stream id-vendor)
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
      (link-list (batched-deque-front-stack deque))
      (link id)
      (link-list (reverse (batched-deque-back-stack deque)))
      id)))

(declaim (inline batched-deque-to-list))
(defun batched-deque-to-list (deque)
  (append (batched-deque-front-stack deque)
          (reverse (batched-deque-back-stack deque))))

(declaim (inline batched-deque-iterator))
(defun batched-deque-iterator (deque)
  (let ((front-iterator (i-iterator <list> (batched-deque-front-stack deque)))
        (back-iterator (i-iterator <list> (reverse (batched-deque-back-stack deque)))))
    (iterator-flatten* front-iterator back-iterator)))

(declaim (inline batched-deque-for-each))
(defun batched-deque-for-each (deque function)
  (i-for-each <list> (batched-deque-front-stack deque) function)
  (i-for-each <list> (reverse (batched-deque-back-stack deque)) function))

(declaim (inline batched-deque-map-members))
(defun batched-deque-map-members (deque function)
  (copy-batched-deque
   deque
   :front-stack (i-map-members <list> (batched-deque-front-stack deque) function)
   :back-stack (nreverse (impure-destructive-mapcar function (reverse (batched-deque-back-stack deque))))))

(defmethod print-object ((deque batched-deque) stream)
  (if *print-readably*
      (call-next-method)
      (print-container <batched-deque> deque stream)))

(defvar *empty-batched-deque* (%make-batched-deque))

(defun split-list (list length)
  (let* ((first-count (ceiling (/ length 2)))
         (second-count (- length first-count))
         (list-builder (make-impure-list-builder)))
    (dotimes (i first-count)
      (impure-list-builder-add list-builder (pop list)))

    (let ((first (impure-list-builder-extract list-builder)))
      (values first first-count list second-count))))

(defun split-front (list length)
  (multiple-value-bind
        (new-front new-front-count new-back new-back-count)
      (split-list list length)
    (values
     new-front
     new-front-count
     (reverse new-back)
     new-back-count)))

(defun split-back (list length)
  (multiple-value-bind
        (new-back new-back-count new-front new-front-count)
      (split-list list length)
    (when (null new-front)
      (assert (equal 1 new-back-count))
      (rotatef new-back new-front)
      (rotatef new-back-count new-front-count))
    (values
     (reverse new-front)
     new-front-count
     new-back
     new-back-count)))

(declaim (inline make-batched-deque))
(defun make-batched-deque (&key items)
  (unless items
    (return-from make-batched-deque
      *empty-batched-deque*))

  (multiple-value-bind
        (front-stack front-count back-stack back-count)
      (split-front items (length items))
    (%make-batched-deque
     :front-stack front-stack
     :front-count front-count
     :back-stack back-stack
     :back-count back-count)))

(declaim (inline batched-deque))
(defun batched-deque (&rest items)
  (make-batched-deque :items items))

(declaim (inline add-last))
(defun add-last (deque item)
  (copy-batched-deque
   deque
   :back-stack (cons item (batched-deque-back-stack deque))
   :back-count (1+ (batched-deque-back-count deque))))

(declaim (inline add-first))
(defun add-first (deque item)
  (copy-batched-deque
   deque
   :front-stack (cons item (batched-deque-front-stack deque))
   :front-count (1+ (batched-deque-front-count deque))))

(declaim (inline batched-deque-with-back))
(defun batched-deque-with-back (deque item)
  (if (batched-deque-front-stack deque)
      (add-last deque item)
      (add-first deque item)))

(defun batched-deque-with-front (deque item)
  (cond
    ((and (equal 0 (batched-deque-back-count deque))
          (equal 1 (batched-deque-front-count deque)))
     (copy-batched-deque
      deque
      :front-stack (cons item nil)
      :front-count 1
      :back-stack (batched-deque-front-stack deque)
      :back-count 1))

    (t
     (add-first deque item))))

(defun batched-deque-without-front (deque)
  (when (null (batched-deque-front-stack deque))
    (assert (null (batched-deque-back-stack deque)))
    (return-from batched-deque-without-front
      (values *empty-batched-deque* nil nil)))

  (let* ((front-stack (batched-deque-front-stack deque))
         (front-count (batched-deque-front-count deque))
         (back-stack (batched-deque-back-stack deque))
         (back-count (batched-deque-back-count deque))
         (value (car front-stack))
         (new-front-stack (cdr front-stack))
         (new-front-count (1- front-count))
         (new-back-stack back-stack)
         (new-back-count back-count))

    (when (and (null new-front-stack)
               (null new-back-stack))
      (return-from batched-deque-without-front
        (values *empty-batched-deque* value t)))

    (when (and (null new-front-stack)
               (not (null new-back-stack)))
      (setf (values new-front-stack new-front-count new-back-stack new-back-count)
            (split-back new-back-stack new-back-count)))

    (values
     (copy-batched-deque
      deque
      :front-stack new-front-stack
      :front-count new-front-count
      :back-stack new-back-stack
      :back-count new-back-count)
     value
     t)))

(defun batched-deque-without-back (deque)
  (when (null (batched-deque-back-stack deque))
    (assert (or (equal 1 (batched-deque-front-count deque))
                (equal 0 (batched-deque-front-count deque))))
    (return-from batched-deque-without-back
      (if (batched-deque-front-stack deque)
          (values *empty-batched-deque* (car (batched-deque-front-stack deque)) t)
          (values deque nil nil))))

  (let* ((front-stack (batched-deque-front-stack deque))
         (front-count (batched-deque-front-count deque))
         (back-stack (batched-deque-back-stack deque))
         (back-count (batched-deque-back-count deque))
         (value (car back-stack))
         (new-front-stack front-stack)
         (new-front-count front-count)
         (new-back-stack (cdr back-stack))
         (new-back-count (1- back-count)))

    (when (and (null new-front-stack)
               (null new-back-stack))
      (return-from batched-deque-without-back
        (values *empty-batched-deque* value t)))

    (when (and (null new-back-stack)
               (not (null new-front-stack)))
      (setf (values new-front-stack new-front-count new-back-stack new-back-count)
            (split-front new-front-stack new-front-count)))

    (values
     (copy-batched-deque
      deque
      :front-stack new-front-stack
      :front-count new-front-count
      :back-stack new-back-stack
      :back-count new-back-count)
     value
     t)))

(declaim (inline batched-deque-peek-front))
(defun batched-deque-peek-front (deque)
  (if (batched-deque-front-stack deque)
      (values (car (batched-deque-front-stack deque))
              t)
      (values nil nil)))

(declaim (inline batched-deque-peek-back))
(defun batched-deque-peek-back (deque)
  (cond
    ((batched-deque-back-stack deque)
     (values (car (batched-deque-back-stack deque))
             t))
    ((batched-deque-front-stack deque)
     (assert (equal 1 (batched-deque-front-count deque)))
     (values (car (batched-deque-front-stack deque))
             t))
    (t
     (values nil nil))))

(declaim (inline batched-deque-is-empty))
(defun batched-deque-is-empty (deque)
  (null (batched-deque-front-stack deque)))

(declaim (inline batched-deque-empty))
(defun batched-deque-empty (deque)
  (declare (ignore deque))
  *empty-batched-deque*)

(declaim (inline batched-deque-size))
(defun batched-deque-size (deque)
  (+ (batched-deque-front-count deque)
     (batched-deque-back-count deque)))

(named-specialize*
  (batched-deque-compare (compare-collection-contents <batched-deque>)))

(declaim (inline batched-deque-with-member))
(defun batched-deque-with-member (deque object)
  (batched-deque-with-back deque object))

(declaim (inline batched-deque-decompose))
(defun batched-deque-decompose (deque)
  (batched-deque-without-front deque))

(declaim (inline batched-deque-with-top))
(defun batched-deque-with-top (deque object)
  (batched-deque-with-back deque object))

(declaim (inline batched-deque-without-top))
(defun batched-deque-without-top (deque)
  (batched-deque-without-back deque))

(declaim (inline batched-deque-peek-top))
(defun batched-deque-peek-top (deque)
  (batched-deque-peek-back deque))

(declaim (inline batched-deque-make-stack))
(defun batched-deque-make-stack (&key items)
  (make-batched-deque :items (reverse items)))

(declaim (inline batched-deque-representative-empty))
(defun batched-deque-representative-empty ()
  *empty-batched-deque*)

(define-interface-methods <batched-deque> batched-deque)
