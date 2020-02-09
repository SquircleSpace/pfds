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

(defpackage :pfds.shcl.io/implementation/weight-balanced-tree
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:to-list #:is-empty #:empty #:check-invariants)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc #:cassert)
  (:import-from :pfds.shcl.io/interface/set
   #:with-member #:without-member #:is-member)
  (:import-from :pfds.shcl.io/interface/map
   #:with-entry #:without-entry #:lookup-entry)
  (:import-from :pfds.shcl.io/utility/tree
   #:define-tree #:print-graphviz
   #:node-left #:node-right #:nil-tree-p)
  (:export
   #:is-empty
   #:empty

   #:with-member
   #:without-member
   #:is-member
   #:make-weight-balanced-set
   #:weight-balanced-set-comparator
   #:weight-balanced-set-p
   #:weight-balanced-set

   #:with-entry
   #:without-entry
   #:lookup-entry
   #:make-weight-balanced-map
   #:weight-balanced-map-comparator
   #:weight-balanced-map-p
   #:weight-balanced-map))
(in-package :pfds.shcl.io/implementation/weight-balanced-tree)

;; See "Implementing Sets Efficiently in a Functional Language" by
;; Stephen Adams

(defvar *balance-factor* 4)

(defgeneric tree-size (tree))

(defun check-wb-invariants (tree)
  (when (nil-tree-p tree)
    (return-from check-wb-invariants))

  (let* ((left (node-left tree))
         (left-size (tree-size left))
         (right (node-right tree))
         (right-size (tree-size right)))
    (when (<= 2 (+ left-size right-size))
      (cassert (<= right-size (* *balance-factor* left-size)))
      (cassert (<= left-size (* *balance-factor* right-size))))
    (cassert (equal (tree-size tree)
                    (+ 1 left-size right-size)))
    (check-wb-invariants left)
    (check-wb-invariants right)))

(defmacro define-balance-operations (base-name)
  (let ((copy-node-base (intern-conc *package* "COPY-" base-name "-NODE"))
        (nil-type (intern-conc *package* base-name "-NIL"))
        (node-type (intern-conc *package* base-name "-NODE"))
        (node-type-right (intern-conc *package* base-name "-NODE-RIGHT"))
        (node-type-left (intern-conc *package* base-name "-NODE-LEFT"))
        (node-type-size (intern-conc *package* base-name "-NODE-SIZE"))
        (balance (intern-conc *package* base-name "-BALANCE"))
        (rotate-left (gensym "ROTATE-LEFT"))
        (rotate-right (gensym "ROTATE-RIGHT"))
        (rotate-double-left (gensym "ROTATE-DOUBLE-LEFT"))
        (rotate-double-right (gensym "ROTATE-DOUBLE-RIGHT"))
        (size (gensym "SIZE"))
        (copy-node+ (gensym "COPY-NODE+")))
    `(progn
       (declaim (inline ,size))
       (defun ,size (tree)
         (etypecase tree
           (,nil-type
            0)
           (,node-type
            (,node-type-size tree))))

       (declaim (inline ,copy-node+))
       (defun ,copy-node+ (tree &key (left (,node-type-left tree)) (right (,node-type-right tree)))
         (,copy-node-base tree :left left :right right :size (+ 1 (,size left) (,size right))))

       (defun ,rotate-left (tree left right)
         (,copy-node+ right :left (,copy-node+ tree :left left :right (,node-type-left right))))

       (defun ,rotate-right (tree left right)
         (,copy-node+ left :right (,copy-node+ tree :left (,node-type-right left) :right right)))

       (defun ,rotate-double-left (tree left right)
         (let* ((right-left (,node-type-left right))
                (right-left-left (,node-type-left right-left))
                (right-left-right (,node-type-right right-left)))
           (,copy-node+ right-left
                              :left (,copy-node+ tree :left left :right right-left-left)
                              :right (,copy-node+ right :left right-left-right))))

       (defun ,rotate-double-right (tree left right)
         (let* ((left-right (,node-type-right left))
                (left-right-right (,node-type-right left-right))
                (left-right-left (,node-type-left left-right)))
           (,copy-node+ left-right
                              :left (,copy-node+ left :right left-right-left)
                              :right (,copy-node+ tree :left left-right-right :right right))))

       (defun ,balance (tree &key (left (,node-type-left tree)) (right (,node-type-right tree)))
         (let ((left-size (,size left))
               (right-size (,size right)))
           (cond
             ((< (+ left-size right-size) 2)
              (,copy-node+ tree :left left :right right))

             ((> right-size (* *balance-factor* left-size))
              (let ((right-left-size (,size (,node-type-left right)))
                    (right-right-size (,size (,node-type-right right))))
                (if (<= right-left-size right-right-size)
                    (,rotate-left tree left right)
                    (,rotate-double-left tree left right))))

             ((> left-size (* *balance-factor* right-size))
              (let ((left-left-size (,size (,node-type-left left)))
                    (left-right-size (,size (,node-type-right left))))
                (if (<= left-right-size left-left-size)
                    (,rotate-right tree left right)
                    (,rotate-double-right tree left right))))

             (t
              (,copy-node+ tree :left left :right right))))))))

(define-tree wb-set (:map-p nil
                     :insert-left-balancer wb-set-balance
                     :insert-right-balancer wb-set-balance
                     :remove-left-balancer wb-set-balance
                     :remove-right-balancer wb-set-balance)
  (size 1 :type (integer 1)))

(define-balance-operations wb-set)

(define-immutable-structure (weight-balanced-set (:constructor %make-weight-balanced-set))
  (tree (wb-set-nil) :type wb-set)
  (comparator (error "comparator is required")))

(defun make-weight-balanced-set (comparator &key items)
  (%make-weight-balanced-set :comparator comparator :tree (make-wb-set comparator :items items)))

(defun weight-balanced-set (comparator &rest items)
  (make-weight-balanced-set comparator :items items))

(defmethod to-list ((set weight-balanced-set))
  (to-list (weight-balanced-set-tree set)))

(defmethod is-empty ((set weight-balanced-set))
  (wb-set-nil-p (weight-balanced-set-tree set)))

(defmethod empty ((set weight-balanced-set))
  (copy-weight-balanced-set set :tree (wb-set-nil)))

(defmethod tree-size ((empty wb-set-nil))
  0)

(defmethod tree-size ((node wb-set-node))
  (wb-set-node-size node))

(defmethod check-invariants ((set weight-balanced-set))
  (check-wb-set (weight-balanced-set-tree set) (weight-balanced-set-comparator set))
  (check-wb-invariants (weight-balanced-set-tree set)))

(defmethod print-object ((set weight-balanced-set) stream)
  (write `(make-weight-balanced-set ',(weight-balanced-set-comparator set) :items ',(to-list set))
         :stream stream))

(defmethod with-member ((set weight-balanced-set) item)
  (copy-weight-balanced-set set :tree (wb-set-insert (weight-balanced-set-comparator set) (weight-balanced-set-tree set) item)))

(defmethod without-member ((set weight-balanced-set) item)
  (copy-weight-balanced-set set :tree (wb-set-remove (weight-balanced-set-comparator set) (weight-balanced-set-tree set) item)))

(defmethod is-member ((set weight-balanced-set) item)
  (wb-set-lookup (weight-balanced-set-comparator set) (weight-balanced-set-tree set) item))

(defmethod print-graphviz ((set weight-balanced-set) stream id-vendor)
  (print-graphviz (weight-balanced-set-tree set) stream id-vendor))

(define-tree wb-map (:map-p t
                     :insert-left-balancer wb-map-balance
                     :insert-right-balancer wb-map-balance
                     :remove-left-balancer wb-map-balance
                     :remove-right-balancer wb-map-balance)
  (size 1 :type (integer 1)))

(define-balance-operations wb-map)

(define-immutable-structure (weight-balanced-map (:constructor %make-weight-balanced-map))
  (tree (wb-map-nil) :type wb-map)
  (comparator (error "comparator is required")))

(defun make-weight-balanced-map (comparator &key alist plist)
  (%make-weight-balanced-map :comparator comparator :tree (make-wb-map comparator :alist alist :plist plist)))

(defun weight-balanced-map (comparator &rest plist)
  (make-weight-balanced-map comparator :plist plist))

(defmethod to-list ((map weight-balanced-map))
  (to-list (weight-balanced-map-tree map)))

(defmethod is-empty ((map weight-balanced-map))
  (wb-map-nil-p (weight-balanced-map-tree map)))

(defmethod empty ((map weight-balanced-map))
  (copy-weight-balanced-map map :tree (wb-map-nil)))

(defmethod tree-size ((empty wb-map-nil))
  0)

(defmethod tree-size ((node wb-map-node))
  (wb-map-node-size node))

(defmethod check-invariants ((map weight-balanced-map))
  (check-wb-map (weight-balanced-map-tree map) (weight-balanced-map-comparator map))
  (check-wb-invariants (weight-balanced-map-tree map)))

(defmethod print-object ((map weight-balanced-map) stream)
  (write `(make-weight-balanced-map ',(weight-balanced-map-comparator map) :items ',(to-list map))
         :stream stream))

(defmethod with-entry ((map weight-balanced-map) key value)
  (copy-weight-balanced-map map :tree (wb-map-insert (weight-balanced-map-comparator map) (weight-balanced-map-tree map) key value)))

(defmethod without-entry ((map weight-balanced-map) key)
  (copy-weight-balanced-map map :tree (wb-map-remove (weight-balanced-map-comparator map) (weight-balanced-map-tree map) key)))

(defmethod lookup-entry ((map weight-balanced-map) key)
  (wb-map-lookup (weight-balanced-map-comparator map) (weight-balanced-map-tree map) key))

(defmethod print-graphviz ((map weight-balanced-map) stream id-vendor)
  (print-graphviz (weight-balanced-map-tree map) stream id-vendor))
