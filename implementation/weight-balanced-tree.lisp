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
   #:to-list #:is-empty #:empty #:check-invariants #:for-each
   #:size #:iterator #:with-entry #:lookup-entry)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-sets #:compare-maps)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure #:define-adt)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc #:cassert #:quote-if-symbol)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-map #:print-set #:print-vector)
  (:import-from :pfds.shcl.io/interface/set
   #:with-member #:without-member #:is-member)
  (:import-from :pfds.shcl.io/interface/map
   #:with-entry #:without-entry #:lookup-entry)
  (:import-from :pfds.shcl.io/interface/list
   #:head #:with-head #:tail)
  (:import-from :pfds.shcl.io/interface/deque
   #:with-first #:with-last #:without-first
   #:without-last #:peek-first #:peek-last)
  (:import-from :pfds.shcl.io/utility/tree
   #:define-tree #:print-graphviz
   #:node-left #:node-right #:nil-tree-p)
  (:export
   #:is-empty
   #:empty
   #:with-entry
   #:without-entry
   #:lookup-entry

   #:with-member
   #:without-member
   #:is-member
   #:make-weight-balanced-set
   #:weight-balanced-set-comparator
   #:weight-balanced-set-p
   #:weight-balanced-set

   #:make-weight-balanced-map
   #:weight-balanced-map-comparator
   #:weight-balanced-map-p
   #:weight-balanced-map

   #:with-head
   #:head
   #:tail
   #:with-first
   #:with-last
   #:without-first
   #:without-last
   #:peek-first
   #:peek-last
   #:make-weight-balanced-sequence
   #:weight-balanced-sequence
   #:weight-balanced-sequence-p))
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
  (wb-set-to-list (weight-balanced-set-tree set)))

(defmethod for-each ((set weight-balanced-set) function)
  (do-wb-set (key (weight-balanced-set-tree set))
    (funcall function key)))

(defmethod iterator ((set weight-balanced-set))
  (iterator (weight-balanced-set-tree set)))

(defmethod compare-objects ((left weight-balanced-set) (right weight-balanced-set))
  (compare-sets left (weight-balanced-set-comparator left)
                right (weight-balanced-set-comparator right)
                #'compare))

(defmethod is-empty ((set weight-balanced-set))
  (wb-set-nil-p (weight-balanced-set-tree set)))

(defmethod empty ((set weight-balanced-set))
  (copy-weight-balanced-set set :tree (wb-set-nil)))

(defmethod size ((set weight-balanced-set))
  (if (wb-set-nil-p (weight-balanced-set-tree set))
      0
      (wb-set-node-size (weight-balanced-set-tree set))))

(defmethod tree-size ((empty wb-set-nil))
  0)

(defmethod tree-size ((node wb-set-node))
  (wb-set-node-size node))

(defmethod check-invariants ((set weight-balanced-set))
  (check-wb-set (weight-balanced-set-tree set) (weight-balanced-set-comparator set))
  (check-wb-invariants (weight-balanced-set-tree set)))

(defmethod print-object ((set weight-balanced-set) stream)
  (if *print-readably*
      (call-next-method)
      (print-set set stream)))

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
  (wb-map-to-list (weight-balanced-map-tree map)))

(defmethod for-each ((map weight-balanced-map) function)
  (do-wb-map (key value (weight-balanced-map-tree map))
    (funcall function key value)))

(defmethod iterator ((map weight-balanced-map))
  (iterator (weight-balanced-map-tree map)))

(defmethod compare-objects ((left weight-balanced-map) (right weight-balanced-map))
  (compare-maps left (weight-balanced-map-comparator left)
                right (weight-balanced-map-comparator right)
                #'compare #'compare))

(defmethod is-empty ((map weight-balanced-map))
  (wb-map-nil-p (weight-balanced-map-tree map)))

(defmethod empty ((map weight-balanced-map))
  (copy-weight-balanced-map map :tree (wb-map-nil)))

(defmethod size ((map weight-balanced-map))
  (if (wb-map-nil-p (weight-balanced-map-tree map))
      0
      (wb-map-node-size (weight-balanced-map-tree map))))

(defmethod tree-size ((empty wb-map-nil))
  0)

(defmethod tree-size ((node wb-map-node))
  (wb-map-node-size node))

(defmethod check-invariants ((map weight-balanced-map))
  (check-wb-map (weight-balanced-map-tree map) (weight-balanced-map-comparator map))
  (check-wb-invariants (weight-balanced-map-tree map)))

(defmethod print-object ((map weight-balanced-map) stream)
  (if *print-readably*
      (call-next-method)
      (print-map map stream)))

(defmethod with-entry ((map weight-balanced-map) key value)
  (copy-weight-balanced-map map :tree (wb-map-insert (weight-balanced-map-comparator map) (weight-balanced-map-tree map) key value)))

(defmethod without-entry ((map weight-balanced-map) key)
  (copy-weight-balanced-map map :tree (wb-map-remove (weight-balanced-map-comparator map) (weight-balanced-map-tree map) key)))

(defmethod lookup-entry ((map weight-balanced-map) key)
  (wb-map-lookup (weight-balanced-map-comparator map) (weight-balanced-map-tree map) key))

(defmethod print-graphviz ((map weight-balanced-map) stream id-vendor)
  (print-graphviz (weight-balanced-map-tree map) stream id-vendor))

(defvar *wb-seq-nil*)

(defun wb-seq-nil ()
  *wb-seq-nil*)

(define-adt weight-balanced-sequence
    ()
  (wb-seq-nil)
  (wb-seq-node
   (left (wb-seq-nil) :type weight-balanced-sequence)
   (right (wb-seq-nil) :type weight-balanced-sequence)
   (size 1 :type (integer 1))
   (value nil)))

(defvar *wb-seq-nil* (make-wb-seq-nil))

(define-balance-operations wb-seq)

(defun wb-seq-size (tree)
  (etypecase tree
    (wb-seq-nil
     0)
    (wb-seq-node
     (wb-seq-node-size tree))))

(defun wb-seq-left-size (tree)
  (wb-seq-size (wb-seq-node-left tree)))

(defun wb-seq-store (tree index value)
  (if (wb-seq-nil-p tree)
      (if (or (equal 0 index)
              (equal -1 index))
          (make-wb-seq-node :value value)
          (error "Index out of bounds!"))
      (let ((left-weight (wb-seq-left-size tree)))
        (cond
          ((< index left-weight)
           (wb-seq-balance tree :left (wb-seq-store (wb-seq-node-left tree) index value)))
          ((= index left-weight)
           (copy-wb-seq-node tree :value value))
          (t
           (wb-seq-balance tree :right (wb-seq-store (wb-seq-node-right tree) (- index (1+ left-weight)) value)))))))

(defun wb-seq-lookup (tree index)
  (if (wb-seq-nil-p tree)
      (values nil nil)
      (let ((left-weight (wb-seq-left-size tree)))
        (cond
          ((< index left-weight)
           (wb-seq-lookup (wb-seq-node-left tree) index))
          ((= index left-weight)
           (wb-seq-node-value tree))
          (t
           (wb-seq-lookup (wb-seq-node-right tree) (- index (1+ left-weight))))))))

(defun wb-seq-remove-min (tree)
  (cond
    ((wb-seq-nil-p (wb-seq-node-left tree))
     (values tree (wb-seq-node-right tree)))
    (t
     (multiple-value-bind (min without-min) (wb-seq-remove-min (wb-seq-node-left tree))
       (values min (wb-seq-balance tree :left without-min))))))

(defun wb-seq-remove (tree index)
  (if (wb-seq-nil-p tree)
      (error "Index out of bounds: ~A" index)
      (let ((left-weight (wb-seq-left-size tree)))
        (cond
          ((< index left-weight)
           (multiple-value-bind (new-left removed) (wb-seq-remove (wb-seq-node-left tree) index)
             (values (wb-seq-balance tree :left new-left)
                     removed)))
          ((= index left-weight)
           (cond
             ((wb-seq-nil-p (wb-seq-node-left tree))
              (values (wb-seq-node-right tree)
                      (wb-seq-node-value tree)))
             ((wb-seq-nil-p (wb-seq-node-right tree))
              (values (wb-seq-node-left tree)
                      (wb-seq-node-value tree)))
             (t
              (multiple-value-bind (min without-min) (wb-seq-remove-min (wb-seq-node-right tree))
                (values (wb-seq-balance min :right without-min :left (wb-seq-node-left tree))
                        (wb-seq-node-value tree))))))
          (t
           (multiple-value-bind (new-right removed) (wb-seq-remove (wb-seq-node-right tree) (- index (1+ left-weight)))
             (values (wb-seq-balance tree :right new-right)
                     removed)))))))

(defun wb-seq-for-each (tree function)
  (unless (wb-seq-nil-p tree)
    (wb-seq-for-each (wb-seq-node-left tree) function)
    (funcall function (wb-seq-node-value tree))
    (wb-seq-for-each (wb-seq-node-right tree) function)))

(defmethod for-each ((seq weight-balanced-sequence) function)
  (wb-seq-for-each seq function))

(defmethod size ((seq weight-balanced-sequence))
  (wb-seq-size seq))

(defmethod is-empty ((seq weight-balanced-sequence))
  (wb-seq-nil-p seq))

(defmethod empty ((seq weight-balanced-sequence))
  (wb-seq-nil))

(defun wb-seq-iterator (tree)
  (when (wb-seq-nil-p tree)
    (return-from wb-seq-iterator
      (lambda ()
        (values nil nil))))

  (let ((stack (list (wb-seq-node-left tree) tree (wb-seq-node-right tree))))
    (lambda ()
      (loop
        (if (null stack)
            (return (values nil nil))
            (let* ((tip (car stack))
                   (left (first tip))
                   (mid (second tip))
                   (right (third tip)))
              (cond
                (left
                 (setf (first tip) nil)
                 (unless (wb-seq-nil-p left)
                   (push (list (wb-seq-node-left left) left (wb-seq-node-right left))
                         stack)))

                (mid
                 (setf (second tip) nil)
                 (return (values (wb-seq-node-value mid) t)))

                (t
                 (pop stack)
                 (unless (wb-seq-nil-p right)
                   (push (list (wb-seq-node-left right) right (wb-seq-node-right right))
                         stack))))))))))

(defmethod iterator ((seq weight-balanced-sequence))
  (wb-seq-iterator seq))

(defmethod print-object ((seq weight-balanced-sequence) stream)
  (if *print-readably*
      (call-next-method)
      (print-vector seq stream)))

(defun make-weight-balanced-sequence (&key items)
  (let ((result (wb-seq-nil)))
    (dolist (item items)
      (setf result (wb-seq-store result (wb-seq-size result) item)))
    result))

(defun weight-balanced-sequence (&rest items)
  (make-weight-balanced-sequence :items items))

(defmethod with-entry ((seq weight-balanced-sequence) index value)
  (wb-seq-store seq index value))

(defmethod without-entry ((seq weight-balanced-sequence) index)
  (nth-value 0 (wb-seq-remove seq index)))

(defmethod lookup-entry ((seq weight-balanced-sequence) index)
  (wb-seq-lookup seq index))

(defmethod with-head ((seq weight-balanced-sequence) value)
  (wb-seq-store seq (wb-seq-size seq) value))

(defmethod head ((seq weight-balanced-sequence))
  (if (wb-seq-nil-p seq)
      (values nil nil)
      (wb-seq-lookup seq (1- (wb-seq-size seq)))))

(defmethod tail ((seq weight-balanced-sequence))
  (if (wb-seq-nil-p seq)
      (values seq nil nil)
      (multiple-value-bind (new-seq removed-value) (wb-seq-remove seq (1- (wb-seq-size seq)))
        (values new-seq removed-value t))))

(defmethod with-first ((seq weight-balanced-sequence) value)
  (wb-seq-store seq -1 value))

(defmethod with-last ((seq weight-balanced-sequence) value)
  (wb-seq-store seq (wb-seq-size seq) value))

(defmethod without-first ((seq weight-balanced-sequence))
  (if (wb-seq-nil-p seq)
      (values seq nil nil)
      (multiple-value-bind (new-seq removed) (wb-seq-remove seq 0)
        (values new-seq removed t))))

(defmethod without-last ((seq weight-balanced-sequence))
  (if (wb-seq-nil-p seq)
      (values seq nil nil)
      (multiple-value-bind (new-seq removed) (wb-seq-remove seq (1- (wb-seq-size seq)))
        (values new-seq removed t))))

(defmethod peek-first ((seq weight-balanced-sequence))
  (if (wb-seq-nil-p seq)
      (values nil nil)
      (wb-seq-lookup seq 0)))

(defmethod peek-last ((seq weight-balanced-sequence))
  (if (wb-seq-nil-p seq)
      (values nil nil)
      (wb-seq-lookup seq (1- (wb-seq-size seq)))))
