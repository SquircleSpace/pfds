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

(defpackage :pfds.shcl.io/implementation/batched-deque
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/list
   #:impure-destructive-mapcar)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-containers #:iterator-flatten*)
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
   #:batched-deque
   #:batched-deque-p
   #:make-batched-deque))
(in-package :pfds.shcl.io/implementation/batched-deque)

;; See "Purely Functional Data Structures" by Chris Okasaki

(define-immutable-structure (batched-deque (:constructor %make-batched-deque))
  (front-stack nil :type list)
  (front-count 0 :type (integer 0))
  (back-stack nil :type list)
  (back-count 0 :type (integer 0)))

(declare-interface-conformance batched-deque deque)

(defmethod check-invariants ((deque batched-deque))
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

(defmethod print-graphviz ((deque batched-deque) stream id-vendor)
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
      (link-list (batched-deque-front-stack deque))
      (link id)
      (link-list (reverse (batched-deque-back-stack deque)))
      id)))

(defmethod to-list ((deque batched-deque))
  (append (batched-deque-front-stack deque)
          (reverse (batched-deque-back-stack deque))))

(defmethod iterator ((deque batched-deque))
  (let ((front-iterator (iterator (batched-deque-front-stack deque)))
        (back-iterator (iterator (reverse (batched-deque-back-stack deque)))))
    (iterator-flatten* front-iterator back-iterator)))

(defmethod for-each ((deque batched-deque) function)
  (for-each (batched-deque-front-stack deque) function)
  (for-each (reverse (batched-deque-back-stack deque)) function))

(defmethod map-members ((deque batched-deque) function)
  (copy-batched-deque deque
                      :front-stack (map-members (batched-deque-front-stack deque) function)
                      :back-stack (nreverse (impure-destructive-mapcar function (reverse (batched-deque-back-stack deque))))))

(defmethod print-object ((deque batched-deque) stream)
  (if *print-readably*
      (call-next-method)
      (print-container deque stream)))

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

(defun batched-deque (&rest items)
  (make-batched-deque :items items))

(defun add-last (deque item)
  (copy-batched-deque
   deque
   :back-stack (cons item (batched-deque-back-stack deque))
   :back-count (1+ (batched-deque-back-count deque))))

(defun add-first (deque item)
  (copy-batched-deque
   deque
   :front-stack (cons item (batched-deque-front-stack deque))
   :front-count (1+ (batched-deque-front-count deque))))

(defun batched-deque-with-back (deque item)
  (if (batched-deque-front-stack deque)
      (add-last deque item)
      (add-first deque item)))

(defmethod with-back ((deque batched-deque) item)
  (batched-deque-with-back deque item))

(defmethod with-front ((deque batched-deque) item)
  (cond
    ((null (batched-deque-back-stack deque))
     (assert (equal 1 (batched-deque-front-count deque)))
     (copy-batched-deque
      deque
      :front-stack (cons item nil)
      :front-count 1
      :back-stack (batched-deque-front-stack deque)
      :back-count 1))

    (t
     (assert (batched-deque-front-stack deque))
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

(defmethod without-front ((deque batched-deque))
  (batched-deque-without-front deque))

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

(defmethod without-back ((deque batched-deque))
  (batched-deque-without-back deque))

(defmethod peek-front ((deque batched-deque))
  (if (batched-deque-front-stack deque)
      (values (car (batched-deque-front-stack deque))
              t)
      (values nil nil)))

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

(defmethod peek-back ((deque batched-deque))
  (batched-deque-peek-back deque))

(defmethod is-empty ((deque batched-deque))
  (null (batched-deque-front-stack deque)))

(defmethod empty ((deque batched-deque))
  *empty-batched-deque*)

(defmethod size ((deque batched-deque))
  (+ (batched-deque-front-count deque)
     (batched-deque-back-count deque)))

(defmethod compare-objects ((left batched-deque) (right batched-deque))
  (compare-containers left right #'compare))

(defmethod with-member ((deque batched-deque) object)
  (batched-deque-with-back deque object))

(defmethod decompose ((deque batched-deque))
  (batched-deque-without-front deque))

(defmethod with-top ((deque batched-deque) object)
  (batched-deque-with-back deque object))

(defmethod without-top ((deque batched-deque))
  (batched-deque-without-back deque))

(defmethod peek-top ((deque batched-deque))
  (batched-deque-peek-back deque))
