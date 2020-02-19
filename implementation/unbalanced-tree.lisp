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

(defpackage :pfds.shcl.io/implementation/unbalanced-tree
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:is-empty #:empty #:for-each #:iterator
   #:size)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/interface/set
   #:with-member #:without-member #:is-member)
  (:import-from :pfds.shcl.io/interface/map
   #:with-entry #:without-entry #:lookup-entry)
  (:import-from :pfds.shcl.io/utility/tree
   #:define-tree)
  (:export
   #:is-empty
   #:empty
   #:with-member
   #:without-member
   #:is-member
   #:with-entry
   #:without-entry
   #:lookup-entry
   #:unbalanced-set
   #:unbalanced-set-p
   #:unbalanced-comparator
   #:make-unbalanced-set
   #:unbalanced-map
   #:unbalanced-map-p
   #:unbalanced-map-comparator
   #:make-unbalanced-map))
(in-package :pfds.shcl.io/implementation/unbalanced-tree)

;; See "Purely Functional Data Structures" by Chris Okasaki

(define-tree %unbalanced-set-tree (:map-p nil))

(define-immutable-structure (unbalanced-set (:constructor %make-unbalanced-set))
  (tree (%unbalanced-set-tree-nil) :type %unbalanced-set-tree)
  (size 0 :type (integer 0))
  (comparator (error "comparator is required")))

(defun make-unbalanced-set (comparator &key items)
  (multiple-value-bind (tree size) (make-%unbalanced-set-tree comparator :items items)
    (%make-unbalanced-set :tree tree :comparator comparator :size size)))

(defun unbalanced-set (comparator &rest items)
  (make-unbalanced-set comparator :items items))

(defmethod print-object ((set unbalanced-set) stream)
  (write
   (if *print-readably*
       `(make-unbalanced-set ',(unbalanced-set-comparator set) :items ',(to-list set))
       `(unbalanced-set ,(unbalanced-set-comparator set) ,@(to-list set)))
   :stream stream))

(defmethod is-empty ((set unbalanced-set))
  (%unbalanced-set-tree-nil-p (unbalanced-set-tree set)))

(defmethod empty ((set unbalanced-set))
  (copy-unbalanced-set set :tree (%unbalanced-set-tree-nil) :size 0))

(defmethod size ((set unbalanced-set))
  (unbalanced-set-size set))

(defmethod with-member ((set unbalanced-set) item)
  (multiple-value-bind
        (new-tree balance-needed-p count-changed-p)
      (%unbalanced-set-tree-insert (unbalanced-set-comparator set)
                                   (unbalanced-set-tree set)
                                   item)
    (declare (ignore balance-needed-p))
    (copy-unbalanced-set set :tree new-tree
                             :size (if count-changed-p
                                       (1+ (unbalanced-set-size set))
                                       (unbalanced-set-size set)))))

(defmethod without-member ((set unbalanced-set) item)
  (multiple-value-bind
        (new-tree balance-needed-p count-changed-p)
      (%unbalanced-set-tree-remove (unbalanced-set-comparator set)
                                   (unbalanced-set-tree set)
                                   item)
    (declare (ignore balance-needed-p))
    (copy-unbalanced-set set :tree new-tree
                             :size (if count-changed-p
                                       (1- (unbalanced-set-size set))
                                       (unbalanced-set-size set)))))

(defmethod is-member ((set unbalanced-set) item)
  (nth-value 0 (%unbalanced-set-tree-lookup (unbalanced-set-comparator set)
                                            (unbalanced-set-tree set)
                                            item)))

(defmethod to-list ((set unbalanced-set))
  (%unbalanced-set-tree-to-list (unbalanced-set-tree set)))

(defmethod for-each ((set unbalanced-set) function)
  (do-%unbalanced-set-tree (key (unbalanced-set-tree set))
    (funcall function key)))

(defmethod iterator ((set unbalanced-set))
  (iterator (unbalanced-set-tree set)))

(define-tree %unbalanced-map-tree (:map-p t))

(define-immutable-structure (unbalanced-map (:constructor %make-unbalanced-map))
  (tree (%unbalanced-map-tree-nil) :type %unbalanced-map-tree)
  (size 0 :type (integer 0))
  (comparator (error "comparator is required")))

(defun make-unbalanced-map (comparator &key alist plist)
  (multiple-value-bind (tree size) (make-%unbalanced-map-tree comparator :alist alist :plist plist)
    (%make-unbalanced-map :tree tree :comparator comparator :size size)))

(defun unbalanced-map (comparator &rest plist)
  (make-unbalanced-map comparator :plist plist))

(defmethod print-object ((map unbalanced-map) stream)
  (write
   (if *print-readably*
       `(make-unbalanced-map ',(unbalanced-map-comparator map) :alist ',(to-list map))
       `(unbalanced-map ,(unbalanced-map-comparator map)
                        ,@(loop :for pair :in (to-list map)
                                :collect (list (car pair) (cdr pair)))))
   :stream stream))

(defmethod is-empty ((map unbalanced-map))
  (%unbalanced-map-tree-nil-p (unbalanced-map-tree map)))

(defmethod empty ((map unbalanced-map))
  (copy-unbalanced-map map :tree (%unbalanced-map-tree-nil) :size 0))

(defmethod size ((map unbalanced-map))
  (unbalanced-map-size map))

(defmethod with-entry ((map unbalanced-map) key value)
  (multiple-value-bind
        (tree balance-needed-p count-changed-p)
      (%unbalanced-map-tree-insert (unbalanced-map-comparator map)
                                   (unbalanced-map-tree map)
                                   key
                                   value)
    (declare (ignore balance-needed-p))
    (copy-unbalanced-map map :tree tree
                             :size (if count-changed-p
                                       (1+ (unbalanced-map-size map))
                                       (unbalanced-map-size map)))))

(defmethod without-entry ((map unbalanced-map) key)
  (multiple-value-bind
        (tree balance-needed-p count-changed-p)
      (%unbalanced-map-tree-remove (unbalanced-map-comparator map)
                                   (unbalanced-map-tree map)
                                   key)
    (declare (ignore balance-needed-p))
    (copy-unbalanced-map map :tree tree
                             :size (if count-changed-p
                                       (1- (unbalanced-map-size map))
                                       (unbalanced-map-size map)))))

(defmethod lookup-entry ((map unbalanced-map) key)
  (%unbalanced-map-tree-lookup (unbalanced-map-comparator map)
                               (unbalanced-map-tree map)
                               key))

(defmethod for-each ((map unbalanced-map) function)
  (do-%unbalanced-map-tree (key value (unbalanced-map-tree map))
    (funcall function key value)))

(defmethod iterator ((map unbalanced-map))
  (iterator (unbalanced-map-tree map)))

(defmethod to-list ((map unbalanced-map))
  (%unbalanced-map-tree-to-list (unbalanced-map-tree map)))
