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

(defpackage :pfds.shcl.io/batched-deque
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common
   #:to-list #:check-invariants)
  (:import-from :pfds.shcl.io/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/deque
   #:with-last #:without-first #:peek-first #:is-empty #:empty
   #:with-first #:without-last #:peek-last)
  (:import-from :pfds.shcl.io/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:export
   #:with-last
   #:with-first
   #:without-first
   #:without-last
   #:peek-first
   #:peek-last
   #:is-empty
   #:empty
   #:make-batched-deque
   #:make-batched-deque*))
(in-package :pfds.shcl.io/batched-deque)

(define-immutable-structure (batched-deque (:constructor %make-batched-deque))
  (front-stack nil :type list)
  (front-count 0 :type (integer 0))
  (back-stack nil :type list)
  (back-count 0 :type (integer 0)))

(defmethod check-invariants ((deque batched-deque))
  (with-accessors ((front batched-deque-front-stack)
                   (front-count batched-deque-front-count)
                   (back batched-deque-back-stack)
                   (back-count batched-deque-back-count))
      queue
    (cassert (or (null back)
                 front)
             nil "back stack cannot be non-nil unless front is non-nil")
    (cassert (equal front-count (length front))
             nil "front count has the correct value")
    (cassert (equal back-count (length back))
             nil "back count has the correct value")))

(defmethod to-list ((deque batched-deque))
  (append (batched-deque-front-stack deque) (reverse (batched-deque-back-stack deque))))

(defmethod print-object ((deque batched-deque) stream)
  (write
   `(make-batched-deque* :items (quote ,(to-list deque)))
   :stream stream))

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

(defun make-batched-deque* (&key items)
  (unless items
    (return-from make-batched-deque*
      *empty-batched-deque*))

  (multiple-value-bind
        (front-stack front-count back-stack back-count)
      (split-front items (length items))
    (%make-batched-deque
     :front-stack front-stack
     :front-count front-count
     :back-stack back-stack
     :back-count back-count)))

(defun make-batched-deque (&rest items)
  (make-batched-deque* :items items))

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

(defmethod with-last ((deque batched-deque) item)
  (if (batched-deque-front-stack deque)
      (add-last deque item)
      (add-first deque item)))

(defmethod with-first ((deque batched-deque) item)
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

(defmethod without-first ((deque batched-deque))
  (when (null (batched-deque-front-stack deque))
    (assert (null (batched-deque-back-stack deque)))
    (return-from without-first
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
      (return-from without-first
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

(defmethod without-last ((deque batched-deque))
  (when (null (batched-deque-back-stack deque))
    (assert (or (equal 1 (batched-deque-front-count deque))
                (equal 0 (batched-deque-front-count deque))))
    (return-from without-last
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
      (return-from without-last
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

(defmethod peek-first ((deque batched-deque))
  (if (batched-deque-front-stack deque)
      (values (car (batched-deque-front-stack deque))
              t)
      (values nil nil)))

(defmethod peek-last ((deque batched-deque))
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

(defmethod is-empty ((deque batched-deque))
  (null (batched-deque-front-stack deque)))

(defmethod empty ((deque batched-deque))
  *empty-batched-deque*)
