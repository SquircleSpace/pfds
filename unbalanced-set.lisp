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
   #:define-interface #:define-adt #:compare #:define-structure
   #:to-list)
  (:import-from :pfds.shcl.io/set
   #:is-empty #:empty #:with-member #:without-member #:is-member)
  (:import-from :pfds.shcl.io/eql-map
   #:make-eql-map #:eql-map-with #:eql-map-count
   #:eql-map-representative #:eql-map-lookup #:do-eql-map
   #:eql-map-without)
  (:export
   #:with-member
   #:is-member
   #:without-member
   #:is-empty
   #:empty
   
   #:empty-unbalanced-set
   #:make-unbalanced-set
   #:make-unbalanced-set*))
(in-package :pfds.shcl.io/unbalanced-set)

(defvar *tree-nil*)

(defun tree-nil ()
  *tree-nil*)

(define-adt unbalanced-tree
    ()
  (tree-nil)
  (tree-node
   (left (tree-nil) :type unbalanced-tree)
   value
   (right (tree-nil) :type unbalanced-tree)))

(defvar *tree-nil* (make-tree-nil))

(define-structure (uneql-tree-node (:include tree-node)))

(defun tree-node-representative-value (tree)
  (etypecase tree
    (uneql-tree-node
     (eql-map-representative (tree-node-value tree)))
    (tree-node
     (tree-node-value tree))))

(define-structure (unbalanced-set (:constructor %make-unbalanced-set))
  (tree (tree-nil) :type unbalanced-tree)
  (comparator 'compare))

(defun empty-unbalanced-set (comparator)
  (%make-unbalanced-set :comparator comparator))

(defun make-unbalanced-set* (comparator &key items)
  (let ((tree (tree-nil)))
    (dolist (item items)
      (setf tree (tree-insert comparator tree item)))
    (%make-unbalanced-set
     :tree tree
     :comparator comparator)))

(defun make-unbalanced-set (comparator &rest items)
  (make-unbalanced-set* comparator :items items))

(defun do-unbalanced-tree-f (tree fn)
  (when (tree-nil-p tree)
    (return-from do-unbalanced-tree-f))
  (do-unbalanced-tree-f (tree-node-right tree) fn)
  (etypecase tree
    (uneql-tree-node
     (do-eql-map (member value (tree-node-value tree))
       (declare (ignore value))
       (funcall fn member)))
    (tree-node
     (funcall fn (tree-node-value tree))))
  (do-unbalanced-tree-f (tree-node-left tree) fn))

(defmacro do-unbalanced-tree ((member tree &optional result) &body body)
  `(block nil
     (do-unbalanced-tree-f ,tree (lambda (,member) ,@body))
     ,result))

(defun unbalanced-set-to-list (set)
  (let (items)
    (do-unbalanced-tree (item (unbalanced-set-tree set))
      (push item items))
    items))

(defmethod to-list ((set unbalanced-set))
  (unbalanced-set-to-list set))

(defmethod print-object ((object unbalanced-set) stream)
  (let ((items (unbalanced-set-to-list object)))
    (write
     `(make-unbalanced-set* (quote ,(unbalanced-set-comparator object))
                            :items (quote ,items))
     :stream stream)))

(defun tree-node-with-changes (tree &key
                                      (left (tree-node-left tree))
                                      (right (tree-node-right tree))
                                      (value (tree-node-value tree)))
  (etypecase tree
    (uneql-tree-node
     (copy-uneql-tree-node
      tree
      :left left
      :right right
      :value value))
    (tree-node
     (copy-tree-node
      tree
      :left left
      :right right
      :value value))))

(defun tree-node-with-uneql-member (tree item)
  (etypecase tree
    (uneql-tree-node
     (tree-node-with-changes tree :value (eql-map-with (tree-node-value tree) item t)))
    (tree-node
     (assert (not (eq item (tree-node-value tree))))
     (make-uneql-tree-node
      :left (tree-node-left tree)
      :value (make-eql-map (tree-node-value tree) t item t)
      :right (tree-node-right tree)))))

(defun tree-node-without-uneql-member (tree item)
  (etypecase tree
    (uneql-tree-node
     (let ((new-set (eql-map-without (tree-node-value tree) item)))
       (assert (plusp (eql-map-count new-set)))
       (if (> (eql-map-count new-set) 1)
           (tree-node-with-changes tree :value new-set)
           (make-tree-node
            :left (tree-node-left tree)
            :right (tree-node-right tree)
            :value (eql-map-representative new-set)))))
    (tree-node
     (assert (not (eq item (tree-node-value tree))))
     tree)))

(defmethod is-empty ((set unbalanced-set))
  (tree-nil-p (unbalanced-set-tree set)))

(defmethod empty ((set unbalanced-set))
  (empty-unbalanced-set (unbalanced-set-comparator set)))

(defun tree-member (comparator tree item)
  (etypecase tree
    (tree-nil
     nil)
    (tree-node
     (let ((value (tree-node-representative-value tree)))
       (ecase (funcall comparator item value)
         (:less
          (tree-member comparator (tree-node-left tree) item))
         (:greater
          (tree-member comparator (tree-node-right tree) item))
         (:equal
          t)
         (:unequal
          (etypecase tree
            (uneql-tree-node
             (nth-value 1 (eql-map-lookup (tree-node-value tree) item)))
            (tree-node
             nil))))))))

(defmethod is-member ((set unbalanced-set) item)
  (tree-member (unbalanced-set-comparator set) (unbalanced-set-tree set) item))

(defun tree-insert (comparator tree item)
  (etypecase tree
    (tree-nil
     (make-tree-node :value item))
    (tree-node
     (let ((value (tree-node-representative-value tree)))
       (ecase (funcall comparator item value)
         (:less
          (tree-node-with-changes tree :left (tree-insert comparator (tree-node-left tree) item)))
         (:greater
          (tree-node-with-changes tree :right (tree-insert comparator (tree-node-right tree) item)))
         (:equal
          tree)
         (:unequal
          (tree-node-with-uneql-member tree item)))))))

(defmethod with-member ((set unbalanced-set) item)
  (let ((new-tree (tree-insert (unbalanced-set-comparator set) (unbalanced-set-tree set) item)))
    (if (eq new-tree (unbalanced-set-tree set))
        set
        (%make-unbalanced-set :tree new-tree :comparator (unbalanced-set-comparator set)))))

(defun tree-remove-min (tree)
  (unless (tree-nil-p (tree-node-left tree))
    (multiple-value-bind (removed-value new-left) (tree-remove-min (tree-node-left tree))
      (return-from tree-remove-min
        (values removed-value
                (tree-node-with-changes tree :left new-left)))))

  (etypecase tree
    (uneql-tree-node
     (let ((value (eql-map-representative (tree-node-value tree))))
       (values (tree-node-without-uneql-member tree value))))
    (tree-node
     (values (tree-node-value tree) (tree-node-right tree)))))

(defun tree-remove (comparator tree item)
  (etypecase tree
    (tree-nil
     tree)
    (tree-node
     (let ((value (tree-node-representative-value tree)))
       (ecase (funcall comparator item value)
         (:less
          (tree-node-with-changes tree :left (tree-remove comparator (tree-node-left tree) item)))
         (:greater
          (tree-node-with-changes tree :right (tree-remove comparator (tree-node-right tree) item)))
         (:equal
          (etypecase tree
            (uneql-tree-node
             (tree-node-without-uneql-member tree item))
            (tree-node
             (when (tree-nil-p (tree-node-left tree))
               (return-from tree-remove (tree-node-right tree)))
             (when (tree-nil-p (tree-node-right tree))
               (return-from tree-remove (tree-node-left tree)))
             (multiple-value-bind (removed-value new-right) (tree-remove-min (tree-node-right tree))
               (tree-node-with-changes
                tree
                :value removed-value
                :right new-right)))))
         (:unequal
          (tree-node-without-uneql-member tree item)))))))

(defmethod without-member ((set unbalanced-set) item)
  (let ((new-tree (tree-remove (unbalanced-set-comparator set) (unbalanced-set-tree set) item)))
    (if (eq new-tree (unbalanced-set-tree set))
        set
        (%make-unbalanced-set :tree new-tree :comparator (unbalanced-set-comparator set)))))
