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

(uiop:define-package :pfds.shcl.io/implementation/weight-balanced-tree
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/utility/specialization
   #:define-specializable-function
   #:named-specialize*)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-ordered-sets #:compare-maps #:iterator-flatten #:compare-collection-contents)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure #:define-adt)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc #:cassert)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-map #:print-set #:print-sequence)
  (:use :pfds.shcl.io/utility/tree)
  (:export
   #:<weight-balanced-set>
   #:weight-balanced-set
   #:weight-balanced-set-p

   #:<weight-balanced-map>
   #:weight-balanced-map
   #:weight-balanced-map-p

   #:make-weight-balanced-sequence
   #:weight-balanced-sequence
   #:weight-balanced-sequence-p))
(in-package :pfds.shcl.io/implementation/weight-balanced-tree)

;; See...
;; - "Implementing Sets Efficiently in a Functional Language" by
;;   Stephen Adams
;; - FSet https://common-lisp.net/project/fset/Site/
;; - Data.Set https://hackage.haskell.org/package/containers-0.6.4.1/docs/src/Data.Set.Internal.html
;; - "Adams' Trees Revisited" by Milan Straka

(declaim (inline omega-balance))
(defun omega-balance ()
  3)

(declaim (inline alpha-balance))
(defun alpha-balance ()
  2)

(define-interface-function tree-weight (tree)
  :define-generic nil)

(define-specializable-function check-balance (<tree>) (tree)
  (when (i-nil-type-p <tree> tree)
    (return-from check-balance))

  (let* ((left (i-node-left <tree> tree))
         (left-size (i-tree-weight <tree> left))
         (right (i-node-right <tree> tree))
         (right-size (i-tree-weight <tree> right)))
    (when (<= 2 (+ left-size right-size))
      (cassert (<= right-size (* (omega-balance) left-size)))
      (cassert (<= left-size (* (omega-balance) right-size))))
    (cassert (equal (i-tree-weight <tree> tree)
                    (+ 1 left-size right-size)))
    (check-balance <tree> left)
    (check-balance <tree> right)))

(define-specializable-function balance (<tree>) (tree left right)
  (let ((left-size (i-tree-weight <tree> left))
        (right-size (i-tree-weight <tree> right)))
    (cond
      ((< (+ left-size right-size) 2)
       (node-copy <tree> tree left right))

      ((> right-size (* (omega-balance) left-size))
       (let ((right-left-size (i-tree-weight <tree> (i-node-left <tree> right)))
             (right-right-size (i-tree-weight <tree> (i-node-right <tree> right))))
         (if (< right-left-size (* (alpha-balance) right-right-size))
             (tree-rotate-left <tree> tree left right)
             (tree-rotate-double-left <tree> tree left right))))

      ((> left-size (* (omega-balance) right-size))
       (let ((left-left-size (i-tree-weight <tree> (i-node-left <tree> left)))
             (left-right-size (i-tree-weight <tree> (i-node-right <tree> left))))
         (if (< left-right-size (* (alpha-balance) left-left-size))
             (tree-rotate-right <tree> tree left right)
             (tree-rotate-double-right <tree> tree left right))))

      (t
       (node-copy <tree> tree left right)))))

(define-tree-type (wb-set :map-p nil)
  (weight 1 :type (integer 1)))

(defun wb-set-weight (tree)
  (etypecase tree
    (wb-set-nil
     0)
    (wb-set-node
     (wb-set-node-weight tree))))

(define-interface <<wb-set>> (<<base-wb-set>> <<rotations>>)
  tree-weight)

(defun copy-wb-set-node-1+ (node &key (left (wb-set-node-left node))
                                   (right (wb-set-node-right node))
                                   (key (wb-set-node-1-key node)))
  (copy-wb-set-node-1
   node
   :left left
   :right right
   :key key
   :weight (+ 1
              (wb-set-weight left)
              (wb-set-weight right))))

(defun copy-wb-set-node-n+ (node &key (left (wb-set-node-left node))
                                   (right (wb-set-node-right node))
                                   (values (wb-set-node-n-values node)))
  (copy-wb-set-node-n
   node
   :left left
   :right right
   :values values
   :weight (+ 1
              (wb-set-weight left)
              (wb-set-weight right))))

(define-interface-instance <wb-set> <<wb-set>>
  'node-1-copy 'copy-wb-set-node-1+
  'node-n-copy 'copy-wb-set-node-n+

  'insert-left-balancer 'wb-set-balance
  'insert-right-balancer 'wb-set-balance
  'remove-left-balancer 'wb-set-balance
  'remove-right-balancer 'wb-set-balance

  'mutate 'wb-set-mutate

  'rotate-left 'wb-set-rotate-left
  'rotate-right 'wb-set-rotate-right
  'rotate-double-left 'wb-set-rotate-double-left
  'rotate-double-right 'wb-set-rotate-double-right

  'tree-weight 'wb-set-weight)

(named-specialize*
  (wb-set-balance (balance <wb-set>))
  (wb-set-mutate (tree-mutate <wb-set>))
  (wb-set-rotate-left (tree-rotate-left <wb-set>))
  (wb-set-rotate-right (tree-rotate-right <wb-set>))
  (wb-set-rotate-double-left (tree-rotate-double-left <wb-set>))
  (wb-set-rotate-double-right (tree-rotate-double-right <wb-set>))
  (make-wb-set (make-set-tree <wb-set>))
  (wb-set-to-list (tree-to-list <wb-set>))
  (wb-set-for-each (tree-for-each <wb-set>))
  (wb-set-map-members (tree-map-members <wb-set>))
  (wb-set-iterator (tree-iterator <wb-set>))
  (wb-set-check-balance (check-balance <wb-set>))
  (wb-set-check-tree-order (check-tree-order <wb-set>))
  (wb-set-check-tree-count (check-tree-count <wb-set>)))

(define-immutable-structure (weight-balanced-set (:constructor %make-weight-balanced-set))
  (tree (make-wb-set-nil) :type wb-set)
  ;; The nodes of the tree store the "weight", not the element count!
  ;; Weight is a measure of the size of the tree itself and ignores
  ;; multiplicity due to unequal items.
  (size 0 :type (integer 0))
  (comparator (error "comparator is required")))

(define-interface <<weight-balanced-set>> (<<ordered-set>> <<tree-wrapper>>))

(define-simple-interface-instance <weight-balanced-set> <<weight-balanced-set>> weight-balanced-set-
  'make-wrapper '%make-weight-balanced-set
  'copy-wrapper 'copy-weight-balanced-set
  'make-ordered-set 'make-weight-balanced-set
  'representative-empty 'make-weight-balanced-set)

(named-specialize*
  (make-weight-balanced-set (wrapper-make-set <weight-balanced-set> <wb-set>))
  (weight-balanced-set-to-list (wrapper-to-list <weight-balanced-set> <wb-set>))
  (weight-balanced-set-for-each (wrapper-for-each <weight-balanced-set> <wb-set>))
  (weight-balanced-set-map-members (wrapper-map-members <weight-balanced-set> <wb-set>))
  (weight-balanced-set-iterator (wrapper-iterator <weight-balanced-set> <wb-set>))
  (weight-balanced-set-compare (wrapper-set-compare <weight-balanced-set>))
  (weight-balanced-set-is-empty (wrapper-is-empty <weight-balanced-set> <wb-set>))
  (weight-balanced-set-empty (wrapper-empty <weight-balanced-set> <wb-set>))
  (weight-balanced-set-check-invariants-base (wrapper-check-invariants <weight-balanced-set> <wb-set>))
  (weight-balanced-set-with-member (wrapper-with-member <weight-balanced-set> <wb-set>))
  (weight-balanced-set-decompose (wrapper-decompose <weight-balanced-set> <wb-set>))
  (weight-balanced-set-without-member (wrapper-without-member <weight-balanced-set> <wb-set>))
  (weight-balanced-set-is-member (wrapper-is-member <weight-balanced-set> <wb-set>))
  (weight-balanced-set-print-graphviz (wrapper-print-graphviz <weight-balanced-set> <wb-set>)))

(define-interface-methods <weight-balanced-set> weight-balanced-set)

(defmethod print-object ((set weight-balanced-set) stream)
  (if *print-readably*
      (call-next-method)
      (print-set <weight-balanced-set> set stream)))

(defun weight-balanced-set-check-invariants (set)
  (weight-balanced-set-check-invariants-base set)
  (wb-set-check-balance (weight-balanced-set-tree set)))

(defun weight-balanced-set (&rest items)
  (make-weight-balanced-set #'compare :items items))

(define-tree-type (wb-map :map-p t)
  (weight 1 :type (integer 1)))

(defun wb-map-weight (tree)
  (etypecase tree
    (wb-map-nil
     0)
    (wb-map-node
     (wb-map-node-weight tree))))

(define-interface <<wb-map>> (<<base-wb-map>> <<rotations>>)
  tree-weight)

(defun copy-wb-map-node-1+ (node &key (left (wb-map-node-left node))
                                   (right (wb-map-node-right node))
                                   (key (wb-map-node-1-key node)))
  (copy-wb-map-node-1
   node
   :left left
   :right right
   :key key
   :weight (+ 1
              (wb-map-weight left)
              (wb-map-weight right))))

(defun copy-wb-map-node-n+ (node &key (left (wb-map-node-left node))
                                   (right (wb-map-node-right node))
                                   (values (wb-map-node-n-values node)))
  (copy-wb-map-node-n
   node
   :left left
   :right right
   :values values
   :weight (+ 1
              (wb-map-weight left)
              (wb-map-weight right))))

(define-interface-instance <wb-map> <<wb-map>>
  'node-1-copy 'copy-wb-map-node-1+
  'node-n-copy 'copy-wb-map-node-n+

  'insert-left-balancer 'wb-map-balance
  'insert-right-balancer 'wb-map-balance
  'remove-left-balancer 'wb-map-balance
  'remove-right-balancer 'wb-map-balance

  'mutate 'wb-map-mutate

  'rotate-left 'wb-map-rotate-left
  'rotate-right 'wb-map-rotate-right
  'rotate-double-left 'wb-map-rotate-double-left
  'rotate-double-right 'wb-map-rotate-double-right

  'tree-weight 'wb-map-weight)

(named-specialize*
  (wb-map-balance (balance <wb-map>))
  (wb-map-mutate (tree-mutate <wb-map>))
  (wb-map-rotate-left (tree-rotate-left <wb-map>))
  (wb-map-rotate-right (tree-rotate-right <wb-map>))
  (wb-map-rotate-double-left (tree-rotate-double-left <wb-map>))
  (wb-map-rotate-double-right (tree-rotate-double-right <wb-map>))
  (make-wb-map (make-map-tree <wb-map>))
  (wb-map-to-list (tree-to-list <wb-map>))
  (wb-map-for-each (tree-for-each <wb-map>))
  (wb-map-map-members (tree-map-members <wb-map>))
  (wb-map-iterator (tree-iterator <wb-map>))
  (wb-map-check-balance (check-balance <wb-map>))
  (wb-map-check-tree-order (check-tree-order <wb-map>))
  (wb-map-check-tree-count (check-tree-count <wb-map>)))

(define-immutable-structure (weight-balanced-map (:constructor %make-weight-balanced-map))
  (tree (make-wb-map-nil) :type wb-map)
  ;; The nodes of the tree store the "weight", not the element count!
  ;; Weight is a measure of the size of the tree itself and ignores
  ;; multiplicity due to unequal items.
  (size 0 :type (integer 0))
  (comparator (error "comparator is required")))

(define-interface <<weight-balanced-map>> (<<ordered-map>> <<tree-wrapper>>))

(define-simple-interface-instance <weight-balanced-map> <<weight-balanced-map>> weight-balanced-map-
  'make-wrapper '%make-weight-balanced-map
  'copy-wrapper 'copy-weight-balanced-map
  'make-ordered-map 'make-weight-balanced-map
  'representative-empty 'make-weight-balanced-map)

(named-specialize*
  (make-weight-balanced-map (wrapper-make-map <weight-balanced-map> <wb-map>))
  (weight-balanced-map-to-list (wrapper-to-list <weight-balanced-map> <wb-map>))
  (weight-balanced-map-for-each (wrapper-for-each <weight-balanced-map> <wb-map>))
  (weight-balanced-map-map-members (wrapper-map-members <weight-balanced-map> <wb-map>))
  (weight-balanced-map-iterator (wrapper-iterator <weight-balanced-map> <wb-map>))
  (weight-balanced-map-compare (wrapper-map-compare <weight-balanced-map>))
  (weight-balanced-map-is-empty (wrapper-is-empty <weight-balanced-map> <wb-map>))
  (weight-balanced-map-empty (wrapper-empty <weight-balanced-map> <wb-map>))
  (weight-balanced-map-check-invariants-base (wrapper-check-invariants <weight-balanced-map> <wb-map>))
  (weight-balanced-map-decompose (wrapper-decompose <weight-balanced-map> <wb-map>))
  (weight-balanced-map-print-graphviz (wrapper-print-graphviz <weight-balanced-map> <wb-map>))
  (weight-balanced-map-with-entry (wrapper-with-entry <weight-balanced-map> <wb-map>))
  (weight-balanced-map-lookup-entry (wrapper-lookup-entry <weight-balanced-map> <wb-map>))
  (weight-balanced-map-without-entry (wrapper-without-entry <weight-balanced-map> <wb-map>))
  (weight-balanced-map-for-each-entry (wrapper-for-each-entry <weight-balanced-map> <wb-map>))
  (weight-balanced-map-map-entries (wrapper-map-entries <weight-balanced-map> <wb-map>)))

(define-interface-methods <weight-balanced-map> weight-balanced-map)

(defmethod print-object ((map weight-balanced-map) stream)
  (if *print-readably*
      (call-next-method)
      (print-map <weight-balanced-map> map stream)))

(defun weight-balanced-map-check-invariants (map)
  (weight-balanced-map-check-invariants-base map)
  (wb-map-check-balance (weight-balanced-map-tree map)))

(defun weight-balanced-map (&rest plist)
  (make-weight-balanced-map :plist plist))

;; Weight balanced sequences are weird because they don't use tree
;; nodes all the way down.  That isn't a feature that the
;; DEFINE-TREE-TYPE macro can do for us (yet?).  So... we're on our
;; own to re-implement a bunch of the stuff from the tree module.

(defvar *max-array-length* 32)

(deftype wb-seq ()
  '(or null wb-seq-node simple-array string))

(deftype non-empty-wb-seq ()
  '(or wb-seq-node simple-array string))

(define-immutable-structure (wb-seq-node (:constructor %make-wb-seq-node))
  (left nil :type non-empty-wb-seq)
  (right nil :type non-empty-wb-seq)
  (weight 1 :type (integer 1)))

(defun wb-seq-weight (tree)
  (etypecase tree
    (null
     0)
    (wb-seq-node
     (wb-seq-node-weight tree))
    (array
     (length tree))))

(defun make-wb-seq-node (&key left right)
  (%make-wb-seq-node :left left
                     :right right
                     :weight (+ (wb-seq-weight left)
                                (wb-seq-weight right))))

(defun wb-seq-concat-arrays (left right)
  (if (and (or (stringp left) (null left))
           (or (stringp right) (null right)))
      (concatenate 'string left right)
      (concatenate 'vector left right)))

(defun maybe-stringify (array)
  (if (and (not (stringp array))
           (loop :for object :across array
                 :do (unless (characterp object)
                       (return nil))
                 :finally (return t)))
      (coerce array 'string)
      array))

(defun make-wb-seq-node-splitting-arrays (left right)
  (let ((total-items (+ (length left) (length right))))
    (multiple-value-bind (first-length remainder) (floor total-items 2)
      (labels
          ((get-nth (index)
             (if (>= index (length left))
                 (aref right (- index (length left)))
                 (aref left index))))
        (let* ((first-string-p
                 (loop :for i :below first-length
                       :do (unless (characterp (get-nth i))
                             (return nil))
                       :finally (return t)))
               (second-string-p
                 (loop :for i :from first-length :below total-items
                       :do (unless (characterp (get-nth i))
                             (return nil))
                       :finally (return t)))
               (new-left (if first-string-p
                             (make-array first-length :element-type 'character)
                             (make-array first-length)))
               (new-right (if second-string-p
                              (make-array (+ first-length remainder) :element-type 'character)
                              (make-array (+ first-length remainder))))
               (target-offset 0)
               (target-array new-left))
          (labels
              ((store (item)
                 (when (>= target-offset (length target-array))
                   (assert (eq target-array new-left))
                   (setf target-array new-right)
                   (setf target-offset 0))
                 (setf (aref target-array target-offset) item)
                 (incf target-offset)))
            (declare (dynamic-extent #'store))
            (loop :for item :across left :do
              (store item))
            (loop :for item :across right :do
              (store item)))
          (%make-wb-seq-node :left new-left
                             :right new-right
                             :weight total-items))))))

(defun wb-seq-rotate-left (left right)
  ;; Balance the new child.  Not because we NEED to, but because it
  ;; gives us a convenient way to concatenate arrays.
  (make-wb-seq-node :left (make-balanced-wb-seq left (wb-seq-node-left right))
                    :right (wb-seq-node-right right)))

(defun wb-seq-rotate-double-left (left right)
  (let* ((right-left (wb-seq-node-left right))
         (right-left (if (arrayp right-left)
                         (make-wb-seq-node-splitting-arrays right-left #())
                         right-left))
         (right-left-left (wb-seq-node-left right-left))
         (right-left-right (wb-seq-node-right right-left)))
    ;; Balance the new children Not because we NEED to, but because it
    ;; gives us a convenient way to concatenate arrays.
    (make-wb-seq-node :left (make-balanced-wb-seq left right-left-left)
                      :right (make-balanced-wb-seq right-left-right (wb-seq-node-right right)))))

(defun wb-seq-rotate-right (left right)
  ;; Balance the new child.  Not because we NEED to, but because it
  ;; gives us a convenient way to concatenate arrays.
  (make-wb-seq-node :left (wb-seq-node-left left)
                    :right (make-balanced-wb-seq (wb-seq-node-right left) right)))

(defun wb-seq-rotate-double-right (left right)
  (let* ((left-right (wb-seq-node-right left))
         (left-right (if (arrayp left-right)
                         (make-wb-seq-node-splitting-arrays left-right #())
                         left-right))
         (left-right-right (wb-seq-node-right left-right))
         (left-right-left (wb-seq-node-left left-right)))
    ;; Balance the new children.  Not because we NEED to, but
    ;; because it gives us a convenient way to concatenate arrays.
    (make-wb-seq-node :left (make-balanced-wb-seq (wb-seq-node-left left) left-right-left)
                      :right (make-balanced-wb-seq left-right-right right))))

(defun make-balanced-wb-seq (left right)
  (let* ((left-weight (wb-seq-weight left))
         (right-weight (wb-seq-weight right))
         (total-items (+ left-weight right-weight)))
    (cond
      ((zerop right-weight)
       left)
      ((zerop left-weight)
       right)

      ((and (arrayp left) (arrayp right))
       (cond
         ((<= total-items *max-array-length*)
          (wb-seq-concat-arrays left right))
         ((or (> right-weight (* (omega-balance) left-weight))
              (> left-weight (* (omega-balance) right-weight)))
          (make-wb-seq-node-splitting-arrays left right))
         (t
          (%make-wb-seq-node :left left :right right :weight total-items))))

      ((> right-weight (* (omega-balance) left-weight))
       (let ((right-left-weight (wb-seq-weight (wb-seq-node-left right)))
             (right-right-weight (wb-seq-weight (wb-seq-node-right right))))
         (if (< right-left-weight (* (alpha-balance) right-right-weight))
             (wb-seq-rotate-left left right)
             (wb-seq-rotate-double-left left right))))

      ((> left-weight (* (omega-balance) right-weight))
       (let ((left-left-weight (wb-seq-weight (wb-seq-node-left left)))
             (left-right-weight (wb-seq-weight (wb-seq-node-right left))))
         (if (< left-right-weight (* (alpha-balance) left-left-weight))
             (wb-seq-rotate-right left right)
             (wb-seq-rotate-double-right left right))))

      (t
       (%make-wb-seq-node :left left :right right :weight total-items)))))

(defun wb-seq-left-weight (tree)
  (wb-seq-weight (wb-seq-node-left tree)))

(defun wb-seq-store (tree index value)
  (cond
    ((null tree)
     (error "Index out of bounds!"))
    ((arrayp tree)
     (cond
       ((eql value (aref tree index))
        tree)
       ((and (stringp tree)
             (characterp value))
        (let ((result (copy-seq tree)))
          (setf (aref result index) value)
          result))
       (t
        (let ((result (if (stringp tree)
                          (coerce tree '(vector t))
                          (copy-seq tree))))
          (setf (aref result index) value)
          (if (characterp value)
              (maybe-stringify result)
              result)))))
    (t
     (let ((left-weight (wb-seq-left-weight tree)))
       (cond
         ((< index left-weight)
          (let* ((old-left (wb-seq-node-left tree))
                 (new-left (wb-seq-store old-left index value)))
            (if (eq new-left old-left)
                tree
                (make-balanced-wb-seq new-left (wb-seq-node-right tree)))))
         (t
          (let* ((old-right (wb-seq-node-right tree))
                 (new-right (wb-seq-store old-right (- index left-weight) value)))
            (if (eq old-right new-right)
                tree
                (make-balanced-wb-seq (wb-seq-node-left tree) new-right)))))))))

(defun wb-seq-concatenate (left right)
  (cond
    ((null left)
     right)
    ((null right)
     left)

    ((and (wb-seq-node-p left)
          (> (wb-seq-weight left) (* (omega-balance) (wb-seq-weight right))))
     (make-balanced-wb-seq (wb-seq-node-left left)
                           (wb-seq-concatenate (wb-seq-node-right left) right)))

    ((and (wb-seq-node-p right)
          (> (wb-seq-weight right) (* (omega-balance) (wb-seq-weight left))))
     (make-balanced-wb-seq (wb-seq-concatenate left (wb-seq-node-left right))
                           (wb-seq-node-right right)))

    (t
     (make-balanced-wb-seq left right))))

(defun wb-seq-with-first (sequence object)
  (wb-seq-insert sequence 0 object))

(defun wb-seq-with-last (sequence object)
  (wb-seq-insert sequence (wb-seq-weight sequence) object))

(defun wb-seq-remove (tree index)
  (etypecase tree
    (array
     (assert (and (>= index 0)
                  (< index (length tree))))
     (cond
       ((equal 1 (length tree))
        (values nil (aref tree 0)))

       (t
        (let ((new-array (if (stringp tree)
                             (make-array (1- (length tree)) :element-type 'character)
                             (make-array (1- (length tree))))))
          (loop :for i :below index :do
            (setf (aref new-array i) (aref tree i)))
          (loop :for i :from (1+ index) :below (length tree) :do
            (setf (aref new-array (1- i)) (aref tree i)))
          (values (maybe-stringify new-array) (aref tree index))))))

    (wb-seq-node
     (let ((left-weight (wb-seq-left-weight tree)))
       (if (>= index left-weight)
           (multiple-value-bind (new-right removed-value) (wb-seq-remove (wb-seq-node-right tree) (- index left-weight))
             (values (make-balanced-wb-seq (wb-seq-node-left tree) new-right) removed-value t))
           (multiple-value-bind (new-left removed-value) (wb-seq-remove (wb-seq-node-left tree) index)
             (values (make-balanced-wb-seq new-left (wb-seq-node-right tree)) removed-value t)))))))

(defun array-insert (array before-index object)
  (cond
    ((<= (1+ (length array)) *max-array-length*)
     (let ((result (make-array (1+ (length array)) :element-type (if (and (stringp array) (characterp object))
                                                                     'character
                                                                     t))))
       (loop :for i :below before-index :do
         (setf (aref result i) (aref array i)))
       (setf (aref result before-index) object)
       (loop :for i :from before-index :below (length array) :do
         (setf (aref result (1+ i)) (aref array i)))
       result))

    (t
     (assert (equal *max-array-length* (length array)))
     (let* ((split-point (floor *max-array-length* 2))
            (first-half (subseq array 0 split-point))
            (second-half (subseq array split-point)))
       (cond
         ((<= before-index split-point)
          (setf second-half (maybe-stringify second-half))
          (setf first-half (array-insert (if (characterp object)
                                             (maybe-stringify first-half)
                                             first-half)
                                         before-index
                                         object)))
         (t
          (setf first-half (maybe-stringify first-half))
          (setf second-half (array-insert (if (characterp object)
                                              (maybe-stringify second-half)
                                              second-half)
                                          (- before-index split-point)
                                          object))))
       (wb-seq-concatenate first-half second-half)))))

(defun wb-seq-insert (tree before-index object)
  (etypecase tree
    (null
     (assert (zerop before-index))
     (if (characterp object)
         (string object)
         (make-array 1 :initial-element object)))

    (array
     (array-insert tree before-index object))

    (wb-seq-node
     (let ((left-weight (wb-seq-left-weight tree)))
       (if (>= before-index left-weight)
           (make-balanced-wb-seq (wb-seq-node-left tree)
                                 (wb-seq-insert (wb-seq-node-right tree) (- before-index left-weight) object))
           (make-balanced-wb-seq (wb-seq-insert (wb-seq-node-left tree) before-index object)
                                 (wb-seq-node-right tree)))))))

(defun wb-seq-lookup (tree index)
  (etypecase tree
    (wb-seq-node
     (let ((left-weight (wb-seq-left-weight tree)))
       (if (>= index left-weight)
           (wb-seq-lookup (wb-seq-node-right tree) (- index left-weight))
           (wb-seq-lookup (wb-seq-node-left tree) index))))
    (array
     (values (aref tree index) t))))

(defun wb-seq-subseq (tree min max)
  (let ((here-weight (wb-seq-weight tree)))
    (when (and (equal 0 min)
               (equal here-weight max))
      (return-from wb-seq-subseq tree)))

  (etypecase tree
    (wb-seq-node
     (let* ((left-weight (wb-seq-left-weight tree))
            (left-touched-p (< min left-weight))
            (right-touched-p (> max left-weight)))
       (cond
         ((and left-touched-p right-touched-p)
          (wb-seq-concatenate (wb-seq-subseq (wb-seq-node-left tree) min left-weight)
                              (wb-seq-subseq (wb-seq-node-right tree) 0 (- max left-weight))))
         (left-touched-p
          (wb-seq-subseq (wb-seq-node-left tree) min max))
         (right-touched-p
          (wb-seq-subseq (wb-seq-node-right tree) (- min left-weight) (- max left-weight))))))
    (array
     (subseq tree min max))))

(defun wb-seq-for-each (tree function offset)
  (etypecase tree
    (wb-seq-node
     (let ((left (wb-seq-node-left tree))
           (right (wb-seq-node-right tree)))
       (wb-seq-for-each left function offset)
       (wb-seq-for-each right function (+ offset (wb-seq-weight left)))))
    (array
     (loop :for object :across tree
           :for index :from offset
           :do (funcall function index object)))
    (null)))

(defun wb-seq-map-entries (tree function offset)
  (etypecase tree
    (wb-seq-node
     (let* ((left (wb-seq-node-left tree))
            (new-left (wb-seq-map-entries left function offset))
            (right (wb-seq-node-right tree))
            (new-right (wb-seq-map-entries right function (+ offset (wb-seq-weight left)))))
       (copy-wb-seq-node tree :left new-left :right new-right)))

    (array
     (let ((new-vector (make-array (length tree)))
           (all-eql t))
       (loop :for object :across tree
             :for index :from offset
             :for vector-index :from 0
             :do (progn
                   (let ((new-value (funcall function index object)))
                     (unless (eql new-value object)
                       (setf all-eql nil))
                     (setf (aref new-vector vector-index) new-value))))
       (if all-eql
           tree
           (maybe-stringify new-vector))))

    (null)))

(define-immutable-structure (weight-balanced-sequence (:constructor %make-weight-balanced-sequnce))
  (tree nil :type wb-seq))

(defvar *empty-weight-balanced-sequence* (%make-weight-balanced-sequnce))

#<MORE>

(defmethod for-each ((seq weight-balanced-sequence) function)
  (wb-seq-for-each (weight-balanced-sequence-tree seq)
                   (lambda (k v)
                     (declare (ignore k))
                     (funcall function v))
                   0))

(defmethod for-each-entry ((seq weight-balanced-sequence) function)
  (wb-seq-for-each (weight-balanced-sequence-tree seq) function 0))

(defmethod map-entries ((seq weight-balanced-sequence) function)
  (copy-weight-balanced-sequence seq :tree (wb-seq-map-entries (weight-balanced-sequence-tree seq) function 0)))

(defmethod map-members ((seq weight-balanced-sequence) function)
  (labels
      ((wrapper (k v)
         (declare (ignore k))
         (funcall function v)))
    (copy-weight-balanced-sequence seq :tree (wb-seq-map-entries (weight-balanced-sequence-tree seq) #'wrapper 0))))

(defmethod size ((seq weight-balanced-sequence))
  (wb-seq-weight (weight-balanced-sequence-tree seq)))

(defmethod is-empty ((seq weight-balanced-sequence))
  (null (weight-balanced-sequence-tree seq)))

(defmethod empty ((seq weight-balanced-sequence))
  *empty-weight-balanced-sequence*)

(defmethod with-member ((seq weight-balanced-sequence) item)
  (%make-weight-balanced-sequnce :tree (wb-seq-with-last (weight-balanced-sequence-tree seq) item)))

(defmethod decompose ((seq weight-balanced-sequence))
  (weight-balanced-sequence-remove-entry seq (1- (wb-seq-weight (weight-balanced-sequence-tree seq)))))

(defun array-iterator (array)
  (let ((offset 0))
    (lambda ()
      (if (>= offset (length array))
          (values nil nil)
          (let ((object (aref array offset)))
            (incf offset)
            (values object t))))))

(defun %wb-seq-iterator (tree)
  (let ((stack (list (list (wb-seq-node-left tree) (wb-seq-node-right tree)))))
    (lambda ()
      (loop
        (if (null stack)
            (return (values nil nil))
            (let* ((tip (car stack))
                   (left (first tip))
                   (right (second tip)))
              (cond
                (left
                 (setf (first tip) nil)
                 (if (wb-seq-node-p left)
                     (push (list (wb-seq-node-left left) (wb-seq-node-right left))
                           stack)
                     (return (values (array-iterator left) t))))
                (right
                 (pop stack)
                 (if (wb-seq-node-p right)
                     (push (list (wb-seq-node-left right) (wb-seq-node-right right))
                           stack)
                     (return (values (array-iterator right) t))))
                (t
                 (assert nil nil "both left and right can't be nil!")))))))))

(defun wb-seq-iterator (tree)
  (etypecase tree
    (null
     (lambda ()
       (values nil nil)))

    (array
     (array-iterator tree))

    (wb-seq-node
     (iterator-flatten (%wb-seq-iterator tree)))))

(defmethod iterator ((seq weight-balanced-sequence))
  (wb-seq-iterator (weight-balanced-sequence-tree seq)))

(defmethod print-object ((seq weight-balanced-sequence) stream)
  (if *print-readably*
      (call-next-method)
      (print-sequence seq stream)))

(defun make-weight-balanced-sequence (&key items)
  (unless items
    (return-from make-weight-balanced-sequence
      *empty-weight-balanced-sequence*))
  (let (tree
        (wip-node (make-array *max-array-length* :fill-pointer 0))
        (wip-node-string-p t))
    (dolist (item items)
      (when (>= (length wip-node) *max-array-length*)
        (setf tree (wb-seq-concatenate tree (if wip-node-string-p
                                                (coerce wip-node 'string)
                                                (make-array (length wip-node) :initial-contents wip-node))))
        (setf wip-node (make-array *max-array-length* :fill-pointer 0))
        (setf wip-node-string-p t))
      (unless (characterp item)
        (setf wip-node-string-p nil))
      (vector-push item wip-node))

    (setf tree (wb-seq-concatenate tree (if wip-node-string-p
                                            (coerce wip-node 'string)
                                            (make-array (length wip-node) :initial-contents wip-node))))

    (%make-weight-balanced-sequnce :tree tree)))

(defun weight-balanced-sequence (&rest items)
  (make-weight-balanced-sequence :items items))

(defmethod with-entry ((seq weight-balanced-sequence) index value)
  (if (equal index (wb-seq-weight (weight-balanced-sequence-tree seq)))
      (%make-weight-balanced-sequnce :tree (wb-seq-with-last (weight-balanced-sequence-tree seq) value))
      (copy-weight-balanced-sequence seq :tree (wb-seq-store (weight-balanced-sequence-tree seq) index value))))

(defun weight-balanced-sequence-remove-entry (seq index)
  (let* ((tree (weight-balanced-sequence-tree seq))
         (weight (wb-seq-weight tree)))
    (cond
      ((or (< index 0)
           (>= index weight))
       (values seq nil nil))

      (t
       (multiple-value-bind (new-tree removed-value) (wb-seq-remove tree index)
         (values
          (if new-tree
              (%make-weight-balanced-sequnce :tree new-tree)
              *empty-weight-balanced-sequence*)
          removed-value
          t))))))

(declaim (inline weight-balanced-sequence-size))
(defun weight-balanced-sequence-size (seq)
  (wb-seq-weight (weight-balanced-sequence-tree seq)))

(defun weight-balanced-sequence-lookup-entry (seq index)
  (let* ((tree (weight-balanced-sequence-tree seq))
         (weight (wb-seq-weight tree)))
    (if (or (> 0 index)
            (>= index weight))
        (values nil nil)
        (wb-seq-lookup tree index))))

(defmethod without-entry ((seq weight-balanced-sequence) index)
  (weight-balanced-sequence-remove-entry seq index))

(defmethod lookup-entry ((seq weight-balanced-sequence) index)
  (weight-balanced-sequence-lookup-entry seq index))

(defmethod with-top ((seq weight-balanced-sequence) value)
  (%make-weight-balanced-sequnce :tree (wb-seq-with-last (weight-balanced-sequence-tree seq) value)))

(defmethod peek-top ((seq weight-balanced-sequence))
  (weight-balanced-sequence-lookup-entry seq (1- (weight-balanced-sequence-size seq))))

(defmethod without-top ((seq weight-balanced-sequence))
  (weight-balanced-sequence-remove-entry seq (1- (wb-seq-weight (weight-balanced-sequence-tree seq)))))

(defmethod with-first ((seq weight-balanced-sequence) value)
  (%make-weight-balanced-sequnce :tree (wb-seq-with-first (weight-balanced-sequence-tree seq) value)))

(defmethod with-last ((seq weight-balanced-sequence) value)
  (%make-weight-balanced-sequnce :tree (wb-seq-with-last (weight-balanced-sequence-tree seq) value)))

(defmethod without-first ((seq weight-balanced-sequence))
  (weight-balanced-sequence-remove-entry seq 0))

(defmethod without-last ((seq weight-balanced-sequence))
  (weight-balanced-sequence-remove-entry seq (1- (wb-seq-weight (weight-balanced-sequence-tree seq)))))

(defmethod peek-first ((seq weight-balanced-sequence))
  (weight-balanced-sequence-lookup-entry seq 0))

(defmethod peek-last ((seq weight-balanced-sequence))
  (weight-balanced-sequence-lookup-entry seq (1- (weight-balanced-sequence-size seq))))

(defmethod compare-objects ((left weight-balanced-sequence) (right weight-balanced-sequence))
  (compare-containers left right #'compare))

(defmethod insert ((seq weight-balanced-sequence) before-index object)
  (check-type before-index (integer 0))
  (let ((tree (weight-balanced-sequence-tree seq)))
    (when (> before-index (wb-seq-weight tree))
      (error "before-index is out of bounds: ~A" before-index))
    (%make-weight-balanced-sequnce :tree (wb-seq-insert tree before-index object))))

(defmethod join ((left weight-balanced-sequence) (right weight-balanced-sequence))
  (let ((tree (wb-seq-concatenate (weight-balanced-sequence-tree left)
                                  (weight-balanced-sequence-tree right))))
    (if tree
        (%make-weight-balanced-sequnce :tree tree)
        *empty-weight-balanced-sequence*)))

(defmethod join ((seq weight-balanced-sequence) other)
  (let ((tree (weight-balanced-sequence-tree seq)))
    (do-each (value other)
      (setf tree (wb-seq-with-last tree value)))
    (if tree
        (%make-weight-balanced-sequnce :tree tree)
        *empty-weight-balanced-sequence*)))

(defmethod subsequence ((seq weight-balanced-sequence) min max)
  (check-type min (integer 0))
  (check-type max (or null (integer 0)))
  (let* ((tree (weight-balanced-sequence-tree seq))
         (count (wb-seq-weight tree)))
    (unless max
      (setf max count))

    (when (> max count)
      (error "max is out of bounds: ~A" max))

    (when (< max min)
      (error "bounds are invalid: min ~A, max ~A" min max))

    (cond
      ((equal min max)
       *empty-weight-balanced-sequence*)

      ((and (zerop min)
            (equal max count))
       seq)

      (t
       (%make-weight-balanced-sequnce :tree (wb-seq-subseq tree min max))))))

(defmethod with-front ((seq weight-balanced-sequence) value)
  (%make-weight-balanced-sequnce :tree (wb-seq-with-first (weight-balanced-sequence-tree seq) value)))

(defmethod with-back ((seq weight-balanced-sequence) value)
  (%make-weight-balanced-sequnce :tree (wb-seq-with-last (weight-balanced-sequence-tree seq) value)))

(defmethod without-front ((seq weight-balanced-sequence))
  (weight-balanced-sequence-remove-entry seq 0))

(defmethod without-back ((seq weight-balanced-sequence))
  (weight-balanced-sequence-remove-entry seq (1- (wb-seq-weight (weight-balanced-sequence-tree seq)))))

(defmethod without-front ((seq weight-balanced-sequence))
  (weight-balanced-sequence-remove-entry seq 0))

(defmethod peek-front ((seq weight-balanced-sequence))
  (weight-balanced-sequence-lookup-entry seq 0))

(defmethod peek-back ((seq weight-balanced-sequence))
  (weight-balanced-sequence-lookup-entry seq (1- (weight-balanced-sequence-size seq))))

(defun wb-seq-print-graphviz (tree stream id-vendor)
  (let ((id (next-graphviz-id id-vendor)))
    (etypecase tree
      (null
       (format stream "ID~A [label=\"~A\"]~%" id nil)
       id)
      (wb-seq-node
       (format stream "ID~A [label=\"~A\"]~%" id (wb-seq-weight tree))
       (let ((child-id (wb-seq-print-graphviz (wb-seq-node-left tree) stream id-vendor)))
         (format stream "ID~A -> ID~A~%" id child-id))
       (let ((child-id (wb-seq-print-graphviz (wb-seq-node-right tree) stream id-vendor)))
         (format stream "ID~A -> ID~A~%" id child-id))
       id)
      (array
       (format stream "ID~A [label=\"~A\" shape=box]~%" id tree)
       id))))

(defmethod print-graphviz ((seq weight-balanced-sequence) stream id-vendor)
  (wb-seq-print-graphviz (weight-balanced-sequence-tree seq) stream id-vendor))

(defun check-wb-seq-invariants (tree)
  (etypecase tree
    (null)
    (array
     (cassert (eq tree (maybe-stringify tree))))
    (wb-seq-node
     (let* ((left (wb-seq-node-left tree))
            (left-weight (wb-seq-weight left))
            (right (wb-seq-node-right tree))
            (right-weight (wb-seq-weight right)))
       (cassert (<= right-weight (* (omega-balance) left-weight)))
       (cassert (<= left-weight (* (omega-balance) right-weight)))
       (cassert (equal (wb-seq-weight tree)
                       (+ left-weight right-weight)))
       (check-wb-seq-invariants left)
       (check-wb-seq-invariants right)))))

(defmethod check-invariants ((seq weight-balanced-sequence))
  (check-wb-seq-invariants (weight-balanced-sequence-tree seq)))
