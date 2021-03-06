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

(defpackage :pfds.shcl.io/implementation/splay-tree
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/printer
   #:print-container)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:compare-heaps #:iterator-to-list #:do-iterator)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/utility/tree
   #:define-tree)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc #:cassert)
  (:import-from :pfds.shcl.io/utility/list
   #:list-set-is-member #:list-map-lookup)
  (:import-from :pfds.shcl.io/utility/structure-mop
   #:define-struct)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:export
   #:impure-splay-set
   #:make-impure-splay-set
   #:copy-impure-splay-set
   #:impure-splay-set-p
   #:impure-splay-set-comparator
   #:impure-splay-set-size
   #:impure-splay-set-is-empty
   #:impure-splay-set-insert
   #:impure-splay-set-remove
   #:impure-splay-set-remove-all
   #:impure-splay-set-is-member
   #:impure-splay-set-to-list

   #:impure-splay-map
   #:make-impure-splay-map
   #:copy-impure-splay-map
   #:impure-splay-map-p
   #:impure-splay-map-comparator
   #:impure-splay-map-size
   #:impure-splay-map-is-empty
   #:impure-splay-map-insert
   #:impure-splay-map-remove
   #:impure-splay-map-remove-all
   #:impure-splay-map-lookup
   #:impure-splay-map-to-list

   #:splay-heap
   #:splay-heap-p
   #:make-splay-heap))
(in-package :pfds.shcl.io/implementation/splay-tree)

;; See "Purely Functional Data Structures" by Chris Okasaki, and
;; https://en.wikipedia.org/wiki/Splay_tree

(declaim (inline max-splay-depth))
(defun max-splay-depth ()
  1000)

(defmacro define-splay (base-name &key
                                    (copier (intern-conc (symbol-package base-name) "COPY-" base-name "-NODE")))
  (let* ((splay-inner (gensym "SPLAY-INNER"))
         (package (symbol-package base-name))
         (splay (intern-conc package base-name "-SPLAY"))
         (representative-key (intern-conc package base-name "-NODE-REPRESENTATIVE"))
         (left (intern-conc package base-name "-NODE-LEFT"))
         (right (intern-conc package base-name "-NODE-RIGHT"))
         (nil-p (intern-conc package base-name "-NIL-P")))
    `(defun ,splay (comparator tree pivot)
       (labels
           ;; This function has a slightly convoluted signature.
           ;; You can think of this function as taking a tree to
           ;; operate on and returning two values: the result of
           ;; performing the splay and how the pivot compared
           ;; against the value that has been splayed to the top of
           ;; the tree.  If the pivot wasn't found then this
           ;; function splays up the closest value encountered on
           ;; the search path.

           ;; In reality, this function actually returns a
           ;; description of the resulting tree using 3 separate
           ;; values: the left child, the central node, and the
           ;; right child.  The fourth return value captures the
           ;; comparison result described earlier.

           ;; Unless we happen to be operating on the root node,
           ;; actually consing up the value prior to return is just
           ;; a waste.  The next call to splay-inner on the stack
           ;; will just want to break apart the tree to perform a
           ;; rotation!  By returning the ingredients that would go
           ;; into making the tree, we avoid that unnecessary
           ;; allocation and give the calling function the
           ;; information it conveniently needed anyway.

           ;; For degenerate trees (i.e. very unbalanced trees), we
           ;; can actually stack overflow trying to get to the
           ;; bottom.  We could work around that by re-working the
           ;; algorithm to be non-recursive and using an explicit
           ;; stack to store state required for splaying on the way
           ;; back up.  Storing our state (a few pointers) in the
           ;; call stack is almost certainly faster than storing it
           ;; in an explicit stack.  It puts less pressure on the
           ;; GC!  So, switching to an explicit stack would probably
           ;; slow down every splay just so that degenerate trees
           ;; won't crash.

           ;; Although this iterative splaying technique will mess
           ;; up the amortized bounds, its probably not a bad thing.
           ;; The end result will be a tree that is more balanced
           ;; than it would have been otherwise.  Since it only
           ;; kicks in when the tree is *very* tall (much taller
           ;; than any balanced tree would plausibly be), the tree
           ;; desperately needs rebalancing anyway.  As long as the
           ;; tree will continue to be used after our repeated
           ;; splaying, the extra balance will save time in the
           ;; future.  Yeah, its hand-wavy.  Sue me.  Or, better
           ;; yet, just don't build degenerate splay trees!
           ((,splay-inner (tree depth)
              (assert (not (,nil-p tree)))

              (let ((first-comparison (funcall comparator (,representative-key tree) pivot)))
                (when (> depth (max-splay-depth))
                  (return-from ,splay-inner
                    (values (,left tree)
                            tree
                            (,right tree)
                            first-comparison
                            nil)))

                (ecase first-comparison
                  ((:equal :unequal)
                   (values (,left tree)
                           tree
                           (,right tree)
                           first-comparison
                           t))

                  (:less
                   (let ((right (,right tree)))
                     (when (,nil-p right)
                       (return-from ,splay-inner
                         (values (,left tree)
                                 tree
                                 right
                                 first-comparison
                                 t)))

                     (let ((second-comparison (funcall comparator (,representative-key right) pivot)))
                       (ecase second-comparison
                         ((:equal :unequal)
                          (values (,copier tree :right (,left right))
                                  right
                                  (,right right)
                                  second-comparison
                                  t))

                         (:less
                          (let ((target (,right right)))
                            (when (,nil-p target)
                              (return-from ,splay-inner
                                (values (,copier tree :right (,left right))
                                        right
                                        (,right right)
                                        second-comparison
                                        t)))

                            (multiple-value-bind (small center big comparison bottom-p) (,splay-inner target (1+ depth))
                              (values
                               (,copier right
                                        :left (,copier tree :right (,left right))
                                        :right small)
                               center
                               big
                               comparison
                               bottom-p))))

                         (:greater
                          (let ((target (,left right)))
                            (when (,nil-p target)
                              (return-from ,splay-inner
                                (values (,copier tree :right (,left right))
                                        right
                                        (,right right)
                                        second-comparison
                                        t)))

                            (multiple-value-bind (small center big comparison bottom-p) (,splay-inner target (1+ depth))
                              (values
                               (,copier tree :right small)
                               center
                               (,copier right :left big)
                               comparison
                               bottom-p))))))))

                  (:greater
                   (let ((left (,left tree)))
                     (when (,nil-p left)
                       (return-from ,splay-inner
                         (values left
                                 tree
                                 (,right tree)
                                 first-comparison
                                 t)))

                     (let ((second-comparison (funcall comparator (,representative-key left) pivot)))
                       (ecase second-comparison
                         ((:unequal :equal)
                          (values (,left left)
                                  left
                                  (,copier tree :left (,right left))
                                  second-comparison
                                  t))

                         (:less
                          (let ((target (,right left)))
                            (when (,nil-p target)
                              (return-from ,splay-inner
                                (values (,left left)
                                        left
                                        (,copier tree :left (,right left))
                                        second-comparison
                                        t)))

                            (multiple-value-bind (small center big comparison bottom-p) (,splay-inner target (1+ depth))
                              (values
                               (,copier left :right small)
                               center
                               (,copier tree :left big)
                               comparison
                               bottom-p))))

                         (:greater
                          (let ((target (,left left)))
                            (when (,nil-p target)
                              (return-from ,splay-inner
                                (values (,left left)
                                        left
                                        (,copier tree :left (,right left))
                                        second-comparison
                                        t)))

                            (multiple-value-bind (small center big comparison bottom-p) (,splay-inner target (1+ depth))
                              (values
                               small
                               center
                               (,copier left
                                        :left big
                                        :right (,copier tree :left (,right left)))
                               comparison
                               bottom-p))))))))))))
         (declare (dynamic-extent #',splay-inner))

         (when (,nil-p tree)
           (return-from ,splay (values tree nil)))

         (loop
           (multiple-value-bind (small center big comparison bottom-p) (,splay-inner tree 0)
             (if bottom-p
                 (return-from ,splay
                   (values (,copier center :left small :right big) comparison))
                 (setf tree (,copier center :left small :right big)))))))))

(defmacro define-splay-tree (base-name (&key map-p)
                             &body extra-node-slots)
  (let* ((splay (intern-conc *package* base-name "-SPLAY"))
         (nil-p (intern-conc *package* base-name "-NIL-P"))
         (nil-type (intern-conc *package* base-name "-NIL"))
         (representative-key (intern-conc *package* base-name "-NODE-REPRESENTATIVE"))
         (left (intern-conc *package* base-name "-NODE-LEFT"))
         (right (intern-conc *package* base-name "-NODE-RIGHT"))
         (node-copy (intern-conc *package* "COPY-" base-name "-NODE"))
         (lookup (intern-conc *package* base-name "-LOOKUP"))
         (node-1-type (intern-conc *package* base-name "-NODE-1"))
         (make-node-1 (intern-conc *package* "MAKE-" node-1-type))
         (node-1-value (when map-p (intern-conc *package* node-1-type "-VALUE")))
         (node-n-type (intern-conc *package* base-name "-NODE-N"))
         (node-n-values (intern-conc *package* node-n-type "-VALUES"))
         (append-children (intern-conc *package* base-name "-APPEND-CHILDREN"))
         (do-tree-f (intern-conc *package* "DO-" base-name "-F"))
         (action (gensym "ACTION"))
         (action-function (gensym "ACTION-FUNCTIONS"))
         (comparison (gensym "COMPARISON"))
         (comparator (gensym "COMPARATOR"))
         (extracted-key (gensym "EXTRACTED-KEY"))
         (extracted-value (gensym "EXTRACTED-VALUE"))
         (key (gensym "KEY"))
         (value (gensym "VALUE"))
         (mutate (intern-conc *package* base-name "-MUTATE"))
         (mutate-equal (intern-conc *package* base-name "-MUTATE-EQUAL"))
         (mutate-unequal (intern-conc *package* base-name "-MUTATE-UNEQUAL"))
         (mutate-other (intern-conc *package* base-name "-MUTATE-OTHER"))
         (splayed (gensym "SPLAYED"))
         (tree (gensym "TREE"))
         (fn (gensym "FN")))
    `(progn
       (define-tree ,base-name (:map-p ,map-p
                                :define-mutate-p nil
                                :mutate ,mutate
                                :define-mutate-equal-p t
                                :mutate-equal ,mutate-equal
                                :define-mutate-unequal-p t
                                :mutate-unequal ,mutate-unequal
                                :define-mutate-missing-p t
                                :define-lookup-p nil
                                :define-append-children-p nil
                                :append-children ,append-children
                                :define-do-tree-f nil
                                :do-tree-f ,do-tree-f)
         ,@extra-node-slots)

       (define-splay ,base-name)

       (defun ,mutate-other (,tree ,key ,comparison ,action-function)
         (multiple-value-bind (,action ,@(when map-p `(,value))) (funcall ,action-function)
           (ecase ,action
             (:insert
              (values
               (,make-node-1 :key ,key ,@(when map-p `(:value ,value))
                             :left (ecase ,comparison
                                     (:less (,node-copy ,tree :right (,nil-type)))
                                     (:greater (,left ,tree))
                                     ((nil) (,nil-type)))
                             :right (ecase ,comparison
                                      (:less (,right ,tree))
                                      (:greater (,node-copy ,tree :left (,nil-type)))
                                      ((nil) (,nil-type))))
               1
               t))
             ((:remove nil)
              (values ,tree 0 nil)))))

       (defun ,mutate (,comparator ,tree ,key ,action-function)
         (multiple-value-bind (,splayed ,comparison) (,splay ,comparator ,tree ,key)
           (ecase ,comparison
             (:equal
              (multiple-value-bind (,extracted-key ,@(when map-p `(,extracted-value))) (,representative-key ,splayed)
                (,mutate-equal ,splayed ,extracted-key ,@(when map-p `(,extracted-value)) ,action-function)))
             (:unequal
              (,mutate-unequal ,comparator ,splayed ,key ,action-function))
             ((:less :greater nil)
              (,mutate-other ,splayed ,key ,comparison ,action-function)))))

       (defun ,append-children (,tree)
         (cond
           ((,nil-p (,right ,tree))
            (,left ,tree))
           ((,nil-p (,left ,tree))
            (,right ,tree))
           (t
            (let ((,splayed (,splay (constantly :less) (,right ,tree) nil)))
              (assert (,nil-p (,right ,splayed)))
              (,node-copy ,splayed :right (,left ,tree))))))

       (defun ,lookup (,comparator tree ,key)
         (multiple-value-bind (,splayed comparison) (,splay ,comparator tree ,key)
           (when (or (eq comparison :less)
                     (eq comparison :greater))
             (return-from ,lookup (values ,splayed nil nil)))
           (etypecase ,splayed
             (,node-1-type
              (if (eq comparison :equal)
                  (values
                   ,splayed
                   ,(if map-p
                        `(,node-1-value ,splayed)
                        t)
                   t)
                  (values ,splayed nil nil)))
             (,node-n-type
              ,(if map-p
                   `(multiple-value-bind (result found-p) (list-map-lookup ,comparator (,node-n-values ,splayed) ,key)
                      (values ,splayed result found-p))
                   `(let ((result (list-set-is-member ,comparator (,node-n-values ,splayed) ,key)))
                      (values ,splayed result result)))))))

       (defun ,do-tree-f (,tree ,fn)
         (do-iterator (,value (iterator ,tree))
           (funcall ,fn ,value))))))

(define-splay-tree sp-set (:map-p nil))

(define-splay-tree sp-map (:map-p t))

(define-struct (impure-splay-set (:constructor %make-impure-splay-set))
  (tree (sp-set-nil) :type sp-set)
  (count 0 :type (integer 0)) ;; Using a different slot name so that we don't export the setter when we export the getter
  (comparator (error "comparator is required") :read-only t))

(defun impure-splay-set-size (impure-splay-set)
  (impure-splay-set-count impure-splay-set))

(defmethod check-invariants ((set impure-splay-set))
  (cassert (equal (impure-splay-set-size set)
                  (check-sp-set (impure-splay-set-tree set) (impure-splay-set-comparator set)))))

(defun impure-splay-set-is-empty (splay-set)
  (sp-set-nil-p (impure-splay-set-tree splay-set)))

(defun impure-splay-set-insert (splay-set item)
  (multiple-value-bind
        (tree count-change)
      (sp-set-insert (impure-splay-set-comparator splay-set) (impure-splay-set-tree splay-set) item)
    (setf (impure-splay-set-tree splay-set) tree)
    (incf (impure-splay-set-count splay-set) count-change)
    (values)))

(defun impure-splay-set-remove (splay-set item)
  (multiple-value-bind
        (new-tree value valid-p count-change)
      (sp-set-remove (impure-splay-set-comparator splay-set) (impure-splay-set-tree splay-set) item)
    (declare (ignore value valid-p))
    (setf (impure-splay-set-tree splay-set) new-tree)
    (incf (impure-splay-set-count splay-set) count-change)
    (values)))

(defun impure-splay-set-remove-all (splay-set)
  (setf (impure-splay-set-tree splay-set) (sp-set-nil))
  (setf (impure-splay-set-count splay-set) 0)
  (values))

(defun impure-splay-set-is-member (splay-set item)
  (multiple-value-bind
        (new-tree member-p)
      (sp-set-lookup (impure-splay-set-comparator splay-set) (impure-splay-set-tree splay-set) item)
    (setf (impure-splay-set-tree splay-set) new-tree)
    member-p))

(defun make-impure-splay-set (comparator &key items)
  (multiple-value-bind (tree size) (make-sp-set comparator :items items)
    (%make-impure-splay-set :comparator comparator :tree tree :count size)))

(defun impure-splay-set-to-list (splay-set)
  ;; splay trees can be VERY imbalanced.  We're going to be defensive
  ;; and avoid traversing the tree with recursion.
  (iterator-to-list (iterator (impure-splay-set-tree splay-set))))

(defmethod print-object ((splay-set impure-splay-set) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (splay-set stream :type t)
        (let ((items (impure-splay-set-to-list splay-set)))
          (pprint-logical-block (stream items)
            (dolist (item items)
              (write item :stream stream)
              (format stream " ")
              (pprint-newline :fill stream)))))))

(defmethod print-graphviz ((map impure-splay-set) stream id-vendor)
  (print-graphviz (impure-splay-set-tree map) stream id-vendor))

(define-struct (impure-splay-map (:constructor %make-impure-splay-map))
  (tree (sp-map-nil) :type sp-map)
  (count 0 :type (integer 0))
  (comparator (error "comparator is required")))

(defun impure-splay-map-size (impure-splay-map)
  (impure-splay-map-count impure-splay-map))

(defmethod check-invariants ((map impure-splay-map))
  (cassert (equal (impure-splay-map-size map)
                  (check-sp-map (impure-splay-map-tree map) (impure-splay-map-comparator map)))))

(defun impure-splay-map-is-empty (splay-map)
  (sp-map-nil-p (impure-splay-map-tree splay-map)))

(defun impure-splay-map-insert (splay-map key value)
  (multiple-value-bind
        (new-tree count-change)
      (sp-map-insert (impure-splay-map-comparator splay-map) (impure-splay-map-tree splay-map) key value)
    (setf (impure-splay-map-tree splay-map) new-tree)
    (incf (impure-splay-map-count splay-map) count-change)
    (values)))

(defun impure-splay-map-emplace (splay-map key value)
  (multiple-value-bind
        (new-tree count-change)
      (sp-map-emplace (impure-splay-map-comparator splay-map) (impure-splay-map-tree splay-map) key value)
    (setf (impure-splay-map-tree splay-map) new-tree)
    (incf (impure-splay-map-count splay-map) count-change)
    (values)))

(defun impure-splay-map-remove (splay-map key)
  (multiple-value-bind
        (new-tree value valid-p count-change)
      (sp-map-remove (impure-splay-map-comparator splay-map) (impure-splay-map-tree splay-map) key)
    (declare (ignore value valid-p))
    (setf (impure-splay-map-tree splay-map) new-tree)
    (incf (impure-splay-map-count splay-map) count-change)
    (values)))

(defun impure-splay-map-remove-all (splay-map)
  (setf (impure-splay-map-tree splay-map) (sp-map-nil))
  (setf (impure-splay-map-count splay-map) 0)
  (values))

(defun impure-splay-map-lookup (splay-map key)
  (multiple-value-bind
        (new-tree value found-p)
      (sp-map-lookup (impure-splay-map-comparator splay-map) (impure-splay-map-tree splay-map) key)
    (setf (impure-splay-map-tree splay-map) new-tree)
    (values value found-p)))

(defun make-impure-splay-map (comparator &key alist plist)
  (multiple-value-bind (tree size) (make-sp-map comparator :alist alist :plist plist)
    (%make-impure-splay-map :comparator comparator :count size :tree tree)))

(defun impure-splay-map-to-list (splay-map)
  ;; splay trees can be VERY imbalanced.  We're going to be defensive
  ;; and avoid traversing the tree with recursion.
  (iterator-to-list (iterator (impure-splay-map-tree splay-map))))

(defmethod print-object ((splay-map impure-splay-map) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (splay-map stream :type t)
        (let ((items (impure-splay-map-to-list splay-map)))
          (pprint-logical-block (stream items)
            (dolist (item items)
              (write item :stream stream)
              (format stream " ")
              (pprint-newline :fill stream)))))))

(defmethod print-graphviz ((map impure-splay-map) stream id-vendor)
  (print-graphviz (impure-splay-map-tree map) stream id-vendor))

(define-tree sp-heap (:map-p nil
                      :define-mutate-p nil
                      :define-lookup-p nil
                      :define-append-children-p nil
                      :define-maker-p nil
                      :maker make-sp-heap
                      :define-do-tree-f nil
                      :define-checker-p nil
                      :do-tree-f do-sp-heap-f)
  (min (error "min is required")))

(defun sp-heap-node-update (base &key (left (sp-heap-node-left base)) (right (sp-heap-node-right base)))
  (copy-sp-heap-node base :left left :right right :min (if (sp-heap-nil-p left)
                                                           (sp-heap-node-1-key base)
                                                           (sp-heap-node-min left))))

(defun do-sp-heap-f (tree fn)
  (do-iterator (value (iterator tree))
    (funcall fn value)))

(define-splay sp-heap :copier sp-heap-node-update)

(defun make-sp-heap-node (&key (key (error "key is required")) (left (sp-heap-nil)) (right (sp-heap-nil)))
  (make-sp-heap-node-1 :key key :left left :right right :min (if (sp-heap-nil-p left)
                                                                 key
                                                                 (sp-heap-node-min left))))

(define-immutable-structure (splay-heap (:constructor %make-splay-heap))
  (tree (sp-heap-nil) :type sp-heap)
  (size 0 :type (integer 0))
  (comparator (error "comparator is required")))

(declare-interface-conformance splay-heap priority-queue)

(defun sp-heap-split (comparator sp-heap pivot)
  (when (sp-heap-nil-p sp-heap)
    (return-from sp-heap-split
      (values (sp-heap-nil) (sp-heap-nil))))

  (multiple-value-bind (splayed comparison) (sp-heap-splay comparator sp-heap pivot)
    (ecase comparison
      (:less
       (values (sp-heap-node-update splayed :right (sp-heap-nil))
               (sp-heap-node-right splayed)))
      ((:greater :equal :unequal)
       (values (sp-heap-node-left splayed)
               (sp-heap-node-update splayed :left (sp-heap-nil)))))))

(defun sp-heap-top (sp-heap)
  (if (sp-heap-nil-p sp-heap)
      (values nil nil)
      (values (sp-heap-node-min sp-heap) t)))

(defmethod peek-front ((heap splay-heap))
  (sp-heap-top (splay-heap-tree heap)))

(defun sp-heap-with-member (comparator sp-heap item)
  (when (sp-heap-nil-p sp-heap)
    (return-from sp-heap-with-member
      (make-sp-heap-node :key item)))

  (multiple-value-bind (lesser greater) (sp-heap-split comparator sp-heap item)
    (make-sp-heap-node :key item :left lesser :right greater)))

(defmethod with-member ((heap splay-heap) item)
  (copy-splay-heap heap
                   :tree (sp-heap-with-member (splay-heap-comparator heap)
                                              (splay-heap-tree heap)
                                              item)
                   :size (1+ (splay-heap-size heap))))

(defun sp-heap-without-heap-top (sp-heap)
  (when (sp-heap-nil-p sp-heap)
    (return-from sp-heap-without-heap-top
      (values sp-heap nil nil)))

  (let ((splayed (sp-heap-splay (constantly :greater) sp-heap nil)))
    (assert (sp-heap-nil-p (sp-heap-node-left splayed)))
    (values
     (sp-heap-node-right splayed)
     (sp-heap-node-1-key splayed)
     t)))

(defun splay-heap-without-front (heap)
  (multiple-value-bind
        (new-tree value valid-p)
      (sp-heap-without-heap-top (splay-heap-tree heap))
    (values (copy-splay-heap heap :tree new-tree
                                  :size (if valid-p
                                            (1- (splay-heap-size heap))
                                            (splay-heap-size heap)))
            value
            valid-p)))

(defmethod without-front ((heap splay-heap))
  (splay-heap-without-front heap))

(defmethod decompose ((heap splay-heap))
  (splay-heap-without-front heap))

(defun sp-heap-merge (comparator left right)
  (when (sp-heap-nil-p left)
    (return-from sp-heap-merge right))
  (when (sp-heap-nil-p right)
    (return-from sp-heap-merge left))

  (multiple-value-bind
        (lesser greater)
      (sp-heap-split comparator left (sp-heap-node-1-key right))
    (sp-heap-node-update right :left (sp-heap-merge comparator lesser (sp-heap-node-left right))
                               :right (sp-heap-merge comparator greater (sp-heap-node-right right)))))

(defmethod meld ((first splay-heap) (second splay-heap))
  (unless (eq (splay-heap-comparator first)
              (splay-heap-comparator second))
    (error "Heaps have different comparators"))

  (%make-splay-heap :tree (sp-heap-merge (splay-heap-comparator first)
                                         (splay-heap-tree first)
                                         (splay-heap-tree second))
                    :size (+ (splay-heap-size first)
                             (splay-heap-size second))
                    :comparator (splay-heap-comparator first)))

(defmethod is-empty ((heap splay-heap))
  (sp-heap-nil-p (splay-heap-tree heap)))

(defmethod empty ((heap splay-heap))
  (copy-splay-heap heap :tree (sp-heap-nil) :size 0))

(defmethod size ((set splay-heap))
  (splay-heap-size set))

(defmethod to-list ((heap splay-heap))
  ;; splay trees can be VERY imbalanced.  We're going to be defensive
  ;; and avoid traversing the tree with recursion.
  (iterator-to-list (iterator heap)))

(defmethod for-each ((heap splay-heap) function)
  ;; splay trees can be VERY imbalanced.  We're going to be defensive
  ;; and avoid traversing the tree with recursion.
  (let ((iterator (iterator heap)))
    (loop
      (multiple-value-bind (value valid-p) (funcall iterator)
        (unless valid-p
          (return))
        (funcall function value)))))

(defmethod map-members ((heap splay-heap) function)
  (multiple-value-bind (tree size) (sp-heap-map-members (splay-heap-comparator heap)
                                                        (splay-heap-tree heap)
                                                        function)
    (copy-splay-heap heap :tree tree :size size)))

(defmethod iterator ((heap splay-heap))
  (iterator (splay-heap-tree heap)))

(defmethod compare-objects ((left splay-heap) (right splay-heap))
  (compare-heaps left (splay-heap-comparator left)
                 right (splay-heap-comparator right)
                 #'compare))

(defun check-sp-heap (sp-heap comparator)
  (etypecase sp-heap
    (sp-heap-nil
     0)
    (sp-heap-node-1
     (let ((count 1))
       (labels ((key (heap) (sp-heap-node-1-key heap)))
         (cond
           ((sp-heap-nil-p (sp-heap-node-1-left sp-heap))
            (cassert (eql (sp-heap-node-min sp-heap) (sp-heap-node-1-key sp-heap))
                    (sp-heap) "min slot should equal the key for nodes without a left child"))
           (t
            (cassert (not (eq :less (funcall comparator (key sp-heap) (key (sp-heap-node-1-left sp-heap))))))
            (incf count (check-sp-heap (sp-heap-node-1-left sp-heap) comparator))
            (cassert (eql (sp-heap-node-min sp-heap) (sp-heap-node-min (sp-heap-node-1-left sp-heap)))
                     (sp-heap) "min slot should always equal the left child's min")))
         (unless (sp-heap-nil-p (sp-heap-node-1-right sp-heap))
           (cassert (not (eq :greater (funcall comparator (key sp-heap) (key (sp-heap-node-1-right sp-heap))))))
           (incf count (check-sp-heap (sp-heap-node-right sp-heap) comparator))))
       count))))

(defmethod check-invariants ((heap splay-heap))
  (cassert (equal (splay-heap-size heap)
                  (check-sp-heap (splay-heap-tree heap) (splay-heap-comparator heap)))))

(defun make-sp-heap (comparator &key items)
  (let ((set (sp-heap-nil))
        (count 0))
    (dolist (item items)
      (setf set (sp-heap-with-member comparator set item))
      (incf count))
    (values set count)))

(defun make-splay-heap (comparator &key items)
  (multiple-value-bind (tree size) (make-sp-heap comparator :items items)
    (%make-splay-heap :tree tree :size size :comparator comparator)))

(defun splay-heap (comparator &rest items)
  (make-splay-heap comparator :items items))

(defmethod print-object ((heap splay-heap) stream)
  (if *print-readably*
      (call-next-method)
      (print-container heap stream)))

(defmethod print-graphviz ((heap splay-heap) stream id-vendor)
  (print-graphviz (splay-heap-tree heap) stream id-vendor))

(defmethod comparator ((heap splay-heap))
  (splay-heap-comparator heap))
