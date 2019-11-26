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

(defpackage :pfds.shcl.io/unbalanced-set
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common
   #:define-interface #:is-empty #:empty #:define-adt #:compare
   #:with-member #:without-member #:is-member)
  (:import-from :pfds.shcl.io/eql-set
   #:make-eql-set #:eql-set-with #:eql-set-count
   #:eql-set-representative #:eql-set-contains-p #:do-eql-set
   #:eql-set-without)
  (:export
   #:with-member
   #:is-member
   #:without-member
   #:is-empty
   #:empty
   
   #:empty-unbalanced-set
   #:make-unbalanced-set
   #:make-unbalanced-set*
   #:do-unbalanced-set))
(in-package :pfds.shcl.io/unbalanced-set)

(define-interface set
  with-member
  is-member
  without-member
  is-empty
  empty)

(define-adt %unbalanced-set
    ((comparator #'compare))
  (%unbalanced-set-nil)
  (%unbalanced-set-node
   (left (make-%unbalanced-set-nil))
   value
   (right (make-%unbalanced-set-nil)))
  (%unbalanced-set-uneql-node
   (left (make-%unbalanced-set-nil))
   values
   (right (make-%unbalanced-set-nil))))

(defun empty-unbalanced-set (comparator)
  (make-%unbalanced-set-nil :comparator comparator))

(defun make-unbalanced-set* (comparator &key items)
  (let ((tree (empty-unbalanced-set comparator)))
    (dolist (item items)
      (setf tree (with-member tree item)))
    tree))

(defun make-unbalanced-set (comparator &rest items)
  (make-unbalanced-set* comparator :items items))

(defgeneric do-unbalanced-set-f (tree fn))

(defmethod do-unbalanced-set-f ((tree %unbalanced-set-nil) fn)
  nil)

(defmethod do-unbalanced-set-f ((tree %unbalanced-set-node) fn)
  (funcall fn (%unbalanced-set-node-value tree))
  (do-unbalanced-set-f (%unbalanced-set-node-left tree) fn)
  (do-unbalanced-set-f (%unbalanced-set-node-right tree) fn))

(defmethod do-unbalanced-set-f ((tree %unbalanced-set-uneql-node) fn)
  (do-eql-set (member (%unbalanced-set-uneql-node-values tree))
    (funcall fn member))
  (do-unbalanced-set-f (%unbalanced-set-uneql-node-left tree) fn)
  (do-unbalanced-set-f (%unbalanced-set-uneql-node-right tree) fn))

(defmacro do-unbalanced-set ((member tree &optional result) &body body)
  `(block nil
     (do-unbalanced-set-f ,tree (lambda (,member) ,@body))
     ,result))

(defmethod print-object ((object %unbalanced-set) stream)
  (let (items)
    (do-unbalanced-set (item object)
      (push item items))
    (write
     `(make-unbalanced-set* (quote ,(%unbalanced-set-comparator object))
                            :items (quote ,items))
     :stream stream)))

(defun convert-to-uneql-node (node)
  (make-%unbalanced-set-uneql-node
   :comparator (%unbalanced-set-comparator node)
   :left (%unbalanced-set-node-left node)
   :values (make-eql-set (%unbalanced-set-node-value node))
   :right (%unbalanced-set-node-right node)))

(defun convert-to-regular-node (uneql-node)
  (let* ((values (%unbalanced-set-uneql-node-values uneql-node))
         (value (if (equal 1 (eql-set-count values))
                    (eql-set-representative values)
                    (error "Cannot convert an uneql-node with a non-singluar value.  This node has ~A elements"
                           (eql-set-count values)))))
    (make-%unbalanced-set-node
     :comparator (%unbalanced-set-comparator uneql-node)
     :left (%unbalanced-set-uneql-node-left uneql-node)
     :value value
     :right (%unbalanced-set-uneql-node-right uneql-node))))

(defmethod is-empty ((set %unbalanced-set-nil))
  t)

(defmethod is-empty ((set %unbalanced-set-node))
  nil)

(defmethod is-empty ((set %unbalanced-set-uneql-node))
  nil)

(defmethod empty ((set %unbalanced-set-node))
  (make-%unbalanced-set-nil :comparator (%unbalanced-set-comparator set)))

(defmethod empty ((set %unbalanced-set-uneql-node))
  (make-%unbalanced-set-nil :comparator (%unbalanced-set-comparator set)))

(defmethod empty ((set %unbalanced-set-nil))
  set)

(defmethod is-member ((set %unbalanced-set-nil) item)
  nil)

(defmethod is-member ((set %unbalanced-set-node) item)
  (let ((value (%unbalanced-set-node-value set)))
    (ecase (funcall (%unbalanced-set-comparator set) item value)
      (:less
       (is-member (%unbalanced-set-node-left set) item))
      (:greater
       (is-member (%unbalanced-set-node-right set) item))
      (:equal
       t)
      (:unequal
       nil))))

(defmethod is-member ((set %unbalanced-set-uneql-node) item)
  (let* ((values (%unbalanced-set-uneql-node-values set))
         (value (progn
                  (assert (plusp (eql-set-count values)))
                  (eql-set-representative values))))
    (ecase (funcall (%unbalanced-set-comparator set) item value)
      (:less
       (is-member (%unbalanced-set-uneql-node-left set) item))
      (:greater
       (is-member (%unbalanced-set-uneql-node-right set) item))
      (:equal
       t)
      (:unequal
       (eql-set-contains-p values item)))))

(defmethod with-member ((set %unbalanced-set-nil) item)
  (make-%unbalanced-set-node :comparator (%unbalanced-set-comparator set)
                          :left set
                          :value item
                          :right set))

(defmethod with-member ((set %unbalanced-set-node) item)
  (with-accessors
        ((left %unbalanced-set-node-left)
         (right %unbalanced-set-node-right)
         (value %unbalanced-set-node-value)
         (comparator %unbalanced-set-comparator))
      set
    (ecase (funcall comparator item value)
      (:less
       (let ((new-left (with-member left item)))
         (if (eq new-left left)
             set
             (make-%unbalanced-set-node
              :comparator comparator
              :left new-left
              :right right
              :value value))))
      (:greater
       (let ((new-right (with-member right item)))
         (if (eq new-right right)
             set
             (make-%unbalanced-set-node
              :comparator comparator
              :left left
              :right new-right
              :value value))))
      (:equal
       set)
      (:unequal
       (with-member (convert-to-uneql-node set) item)))))

(defmethod with-member ((set %unbalanced-set-uneql-node) item)
  (with-accessors
        ((left %unbalanced-set-uneql-node-left)
         (right %unbalanced-set-uneql-node-right)
         (values %unbalanced-set-uneql-node-values)
         (comparator %unbalanced-set-comparator))
      set
    (let ((value (progn
                   (assert (plusp (eql-set-count values)))
                   (eql-set-representative values))))
      (ecase (funcall comparator item value)
        (:less
         (let ((new-left (with-member left item)))
           (if (eq new-left left)
               set
               (make-%unbalanced-set-uneql-node
                :comparator comparator
                :left new-left
                :right right
                :values values))))
        (:greater
         (let ((new-right (with-member right item)))
           (if (eq new-right right)
               set
               (make-%unbalanced-set-uneql-node
                :comparator comparator
                :left left
                :right new-right
                :values values))))
        (:equal
         set)
        (:unequal
         (let ((new-values (eql-set-with values item)))
           (if (eq new-values values)
               set
               (make-%unbalanced-set-uneql-node
                :comparator comparator
                :left left
                :right right
                :values new-values))))))))

(defgeneric without-min-node (set))

(defmethod without-min-node ((set %unbalanced-set-node))
  (with-accessors
        ((left %unbalanced-set-node-left)
         (right %unbalanced-set-node-right)
         (value %unbalanced-set-node-value)
         (comparator %unbalanced-set-comparator))
      set
    (cond
      ((not (%unbalanced-set-nil-p left))
       (multiple-value-bind (new-left removed-node) (without-min-node left)
         (values
          (make-%unbalanced-set-node
           :comparator comparator
           :left new-left
           :right right
           :value value)
          removed-node)))

      (t
       (values left set)))))

(defmethod without-min-node ((set %unbalanced-set-uneql-node))
  (with-accessors
        ((left %unbalanced-set-uneql-node-left)
         (right %unbalanced-set-uneql-node-right)
         (values %unbalanced-set-uneql-node-values)
         (comparator %unbalanced-set-comparator))
      set
    (cond
      ((not (%unbalanced-set-nil-p left))
       (multiple-value-bind (new-left removed-node) (without-min-node left)
         (values
          (make-%unbalanced-set-uneql-node
           :comparator comparator
           :left new-left
           :right right
           :values values)
          removed-node)))

      (t
       (values left set)))))

(defmethod without-member ((set %unbalanced-set-nil) item)
  set)

(defmethod without-member ((set %unbalanced-set-node) item)
  (with-accessors
        ((left %unbalanced-set-node-left)
         (right %unbalanced-set-node-right)
         (value %unbalanced-set-node-value)
         (comparator %unbalanced-set-comparator))
      set
    (ecase (funcall comparator item value)
      (:less
       (let ((new-left (without-member left item)))
         (if (eq new-left left)
             set
             (make-%unbalanced-set-node
              :comparator comparator
              :left new-left
              :right right
              :value value))))

      (:greater
       (let ((new-right (without-member right item)))
         (if (eq new-right right)
             set
             (make-%unbalanced-set-node
              :comparator comparator
              :left left
              :right new-right
              :value value))))

      (:equal
       (cond
         ((%unbalanced-set-nil-p left)
          right)

         ((%unbalanced-set-nil-p right)
          left)

         (t
          (multiple-value-bind (new-right removed-node) (without-min-node right)
            (etypecase removed-node
              (%unbalanced-set-node
               (make-%unbalanced-set-node
                :comparator comparator
                :left left
                :right new-right
                :value (%unbalanced-set-node-value removed-node)))

              (%unbalanced-set-uneql-node
               (make-%unbalanced-set-uneql-node
                :comparator comparator
                :left left
                :right new-right
                :values (%unbalanced-set-uneql-node-values removed-node))))))))

      (:unequal
       set))))

(defmethod without-member ((set %unbalanced-set-uneql-node) item)
  (with-accessors
        ((left %unbalanced-set-uneql-node-left)
         (right %unbalanced-set-uneql-node-right)
         (values %unbalanced-set-uneql-node-values)
         (comparator %unbalanced-set-comparator))
      set
    (let ((value (eql-set-representative values)))
      (ecase (funcall comparator item value)
        (:less
         (let ((new-left (without-member left item)))
           (if (eq new-left left)
               set
               (make-%unbalanced-set-uneql-node
                :comparator comparator
                :left new-left
                :right right
                :values values))))

        (:greater
         (let ((new-right (without-member right item)))
           (if (eq new-right right)
               set
               (make-%unbalanced-set-uneql-node
                :comparator comparator
                :left left
                :right new-right
                :values values))))

        ((:equal :unequal)
         (let ((new-values (eql-set-without values item)))
           (when (eq new-values values)
             (return-from without-member set))
           (if (equal 1 (eql-set-count new-values))
               (make-%unbalanced-set-node
                :comparator comparator
                :left left
                :right right
                :value (eql-set-representative new-values))
               (make-%unbalanced-set-uneql-node
                :comparator comparator
                :left left
                :right right
                :values new-values))))))))
