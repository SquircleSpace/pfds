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

(uiop:define-package :pfds.shcl.io/utility/tree
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/utility/iterator-tools
   #:list-iterator #:singleton-iterator
   #:empty-iterator)
  (:import-from :pfds.shcl.io/utility/specialization
   #:define-specializable-function
   #:mutually-recursive-specializable-functions)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-adt #:structure-convert #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc #:cassert)
  (:import-from :pfds.shcl.io/utility/list
   #:list-set-with #:list-set-without #:list-set #:list-set-is-member
   #:list-set-mutate
   #:list-map-with #:list-map-without #:list-map #:list-map-lookup
   #:list-map-map-entries #:list-map-mutate)
  (:export
   #:define-tree #:print-tree-node-properties
   #:nil-tree-p #:node-left #:node-right #:node-values))
(in-package :pfds.shcl.io/utility/tree)

(declaim (inline always-t))
(defun always-t ()
  t)

(define-compiler-macro always-t ()
  t)

(declaim (inline always-nil))
(defun always-nil ()
  nil)

(define-compiler-macro always-nil ()
  nil)

(declaim (inline default-node-1-value))
(defun default-node-1-value (node)
  (declare (ignore node))
  t)

(define-compiler-macro default-node-1-value (node)
  (declare (ignore node))
  t)

(declaim (inline default-prepare-graphviz-properties))
(defun default-prepare-graphviz-properties (tree properties)
  (declare (ignore tree properties))
  nil)

(define-compiler-macro default-prepare-graphviz-properties (tree properties)
  (declare (ignore tree properties))
  nil)

;; Instances of this can be easily generated by a macro
(define-interface <<base-tree>> ()
  nil-type-p
  node-1-type-p
  node-n-type-p
  node-type-p

  node-1-key
  (:optional node-1-value default-node-1-value)
  node-n-values
  node-left
  node-right

  node-1-copy
  node-n-copy
  convert-1-to-n
  convert-n-to-1

  make-node-1
  make-nil

  map-p

  (:optional prepare-graphviz-properties default-prepare-graphviz-properties))

;; Instances of this require special knowledge about the tree's
;; invariants
(define-interface <<tree>> (<<base-tree>>)
  remove-left-balancer
  remove-right-balancer
  insert-left-balancer
  insert-right-balancer

  (:optional mutate tree-mutate))

(defmacro define-tree-type (base-name map-p &rest extra-node-slots)
  (let* ((package *package*)

         (nil-type (intern-conc package base-name "-NIL"))
         (nil-instance (intern-conc package "*" nil-type "*"))
         (make-nil (intern-conc package "MAKE-" nil-type))
         (secret-nil-maker (intern-conc package "%" make-nil))
         (nil-type-p (intern-conc package nil-type "-P"))

         (node-base-type (intern-conc package base-name "-NODE"))
         (node-type-p (intern-conc package node-base-type "-P"))
         (node-left (intern-conc package node-base-type "-LEFT"))
         (node-right (intern-conc package node-base-type "-RIGHT"))

         (node-1-type (intern-conc package node-base-type "-1"))
         (make-node-1 (intern-conc package "MAKE-" node-1-type))
         (node-1-copy (intern-conc package "COPY-" node-1-type))
         (node-1-type-p (intern-conc package node-1-type "-P"))
         (node-1-key (intern-conc package node-1-type "-KEY"))
         (node-1-value (when map-p
                         (intern-conc package node-1-type "-VALUE")))

         (node-n-type (intern-conc package node-base-type "-N"))
         (node-n-copy (intern-conc package "COPY-" node-n-type))
         (node-n-values (intern-conc package node-n-type "-VALUES"))
         (node-n-type-p (intern-conc package node-n-type "-P"))

         (convert-1-to-n (intern-conc package "CONVERT-" node-base-type "-1-TO-N"))
         (convert-n-to-1 (intern-conc package "CONVERT-" node-base-type "-N-TO-1"))

         (interface-name (intern-conc package "<<" base-name ">>"))

         (node-copy (intern-conc package "COPY-" node-base-type))

         (key (make-symbol "KEY"))
         (value (make-symbol "VALUE"))
         (values (make-symbol "VALUES"))
         (tree (make-symbol "TREE"))
         (left (make-symbol "LEFT"))
         (right (make-symbol "RIGHT")))
    `(progn
       (defvar ,nil-instance)

       (defun ,make-nil ()
         ,nil-instance)

       (define-adt ,base-name
           ()
         ((,nil-type (:copier nil)
                     (:constructor ,secret-nil-maker)))
         ((,node-base-type (:copier nil)
                           (:constructor nil))
          (left (,make-nil) :type ,base-name)
          (right (,make-nil) :type ,base-name)
          ,@extra-node-slots))

       (define-immutable-structure (,node-1-type (:include ,node-base-type))
         (key (error "required"))
         ,@(when map-p
             `((value (error "required")))))

       (define-immutable-structure (,node-n-type (:include ,node-base-type))
         (values (error "required")))

       (defvar ,nil-instance (,secret-nil-maker))

       (defun ,convert-1-to-n (,tree &key ,values)
         (structure-convert (,node-n-type ,node-1-type) ,tree :values ,values))

       (defun ,convert-n-to-1 (,tree &key ,key ,@(when map-p `(,value)))
         (structure-convert (,node-1-type ,node-n-type) ,tree
                            :key ,key
                            ,@(when map-p `(:value ,value))))

       (defun ,node-copy (,tree &key (,left (,node-left ,tree))
                                         (,right (,node-right ,tree)))
         (etypecase ,tree
           (,node-1-type
            (,node-1-copy ,tree :left ,left :right ,right))
           (,node-n-type
            (,node-n-copy ,tree :left ,left :right ,right))))

       (define-interface ,interface-name (<<tree>>)
         (:optional nil-type-p ,nil-type-p)
         (:optional node-1-type-p ,node-1-type-p)
         (:optional node-n-type-p ,node-n-type-p)
         (:optional node-type-p ,node-type-p)

         (:optional node-1-key ,node-1-key)
         ,@(when map-p
             `((:optional node-1-value ,node-1-value)))
         (:optional node-n-values ,node-n-values)
         (:optional node-left ,node-left)
         (:optional node-right ,node-right)

         (:optional node-1-copy ,node-1-copy)
         (:optional node-n-copy ,node-n-copy)
         (:optional convert-1-to-n ,convert-1-to-n)
         (:optional convert-n-to-1 ,convert-n-to-1)

         (:optional make-node-1 ,make-node-1)
         (:optional make-nil ,make-nil)

         (:optional map-p ,(if map-p 'always-t 'always-nil))

         (:optional insert-left-balancer ,node-copy)
         (:optional insert-right-balancer ,node-copy)
         (:optional remove-left-balancer ,node-copy)
         (:optional remove-right-balancer ,node-copy))

       ',base-name)))

(define-specializable-function representative-key (<interface>) (node)
  (with-interface <interface>
      (node-1-type-p node-n-type-p node-n-values node-1-key node-1-value
                     map-p)
    (cond
      ((funcall node-1-type-p node)
       (values (funcall node-1-key node)
               (funcall node-1-value node)))

      ((funcall node-n-type-p node)
       (let ((value (car (funcall node-n-values node))))
         (if (funcall map-p)
             (values (car value) (cdr value))
             (values value t))))

      (t
       (error "Argument isn't a node: ~W" node)))))

(define-specializable-function node-copy
    (<interface>)
    (node &key
          (left (funcall (interface-get <interface> 'node-left) node))
          (right (funcall (interface-get <interface> 'node-right) node)))
  (with-interface <interface>
      (node-1-type-p node-n-type-p node-1-copy node-n-copy)
    (cond
      ((funcall node-1-type-p node)
       (funcall node-1-copy node :left left :right right))

      ((funcall node-n-type-p node)
       (funcall node-n-copy node :left left :right right)))))

(define-specializable-function remove-min-node (<interface>) (tree)
  (with-interface <interface>
      (nil-type-p node-type-p node-left node-right remove-left-balancer)
    (cond
      ((funcall nil-type-p tree)
       (error "nil tree has no min"))

      ((funcall node-type-p tree)
       (if (funcall nil-type-p (funcall node-left tree))
           (values tree (funcall node-right tree))
           (multiple-value-bind (result value) (remove-min-node <interface> (funcall node-left tree))
             (values result (funcall remove-left-balancer tree :left value))))))))

(define-specializable-function node-with-the-value-content-of-node
    (<interface>)
    (tree value-provider)
  (with-interface <interface>
      (node-1-type-p node-n-type-p node-1-copy node-n-copy convert-1-to-n convert-n-to-1
                     node-1-key node-1-value node-n-values map-p)
    (if (funcall node-1-type-p tree)
        (if (funcall node-1-type-p value-provider)
            (if (funcall map-p)
                (funcall node-1-copy tree :key (funcall node-1-key value-provider)
                                          :value (funcall node-1-value value-provider))
                (funcall node-1-copy tree :key (funcall node-1-key value-provider)))
            (funcall convert-1-to-n tree :values (funcall node-n-values value-provider)))
        (if (funcall node-1-type-p value-provider)
            (if (funcall map-p)
                (funcall convert-n-to-1 tree :key (funcall node-1-key value-provider) :value (funcall node-1-value value-provider))
                (funcall convert-n-to-1 tree :key (funcall node-1-key value-provider)))
            (funcall node-n-copy tree :values (funcall node-n-values value-provider))))))

(define-specializable-function tree-append-children (<interface>) (tree)
  (with-interface <interface>
      (nil-type-p node-left node-right remove-right-balancer)
    (when (funcall nil-type-p (funcall node-left tree))
      (return-from tree-append-children
        (funcall node-right tree)))
    (when (funcall nil-type-p (funcall node-right tree))
      (return-from tree-append-children
        (funcall node-left tree)))

    (multiple-value-bind (min without-min) (remove-min-node <interface> (funcall node-right tree))
      (funcall remove-right-balancer
               (node-with-the-value-content-of-node
                <interface>
                tree
                min)
               :right without-min))))

(define-specializable-function node-n-copy-with-values (<interface>) (tree values)
  (with-interface <interface>
      (node-n-copy convert-n-to-1 map-p)
    (if (cdr values)
        (funcall node-n-copy tree :values values)
        (if (funcall map-p)
            (funcall convert-n-to-1 tree :key (car (car values)) :value (cdr (car values)))
            (funcall convert-n-to-1 tree :key (car values))))))

(mutually-recursive-specializable-functions
  (define-specializable-function tree-mutate-missing (<interface>) (tree key action-function)
    (multiple-value-bind (action value) (funcall action-function)
      (with-interface <interface>
          (nil-type-p node-1-type-p node-n-type-p make-node-1 convert-1-to-n
                      node-1-key node-1-value node-n-copy node-n-values
                      map-p)
        (ecase action
          (:insert
           (cond
             ((funcall nil-type-p tree)
              (values (if (funcall map-p)
                          (funcall make-node-1 :key key :value value)
                          (funcall make-node-1 :key key))
                      1
                      t))

             ((funcall node-1-type-p tree)
              (values (funcall convert-1-to-n tree
                               :values (if (funcall map-p)
                                           (list
                                            (cons (funcall node-1-key tree) (funcall node-1-value tree))
                                            (cons key value))
                                           (list
                                            (funcall node-1-key tree)
                                            key)))
                      1
                      nil))

             ((funcall node-n-type-p tree)
              (values (funcall node-n-copy tree :values (cons
                                                         (if (funcall map-p)
                                                             (cons key value)
                                                             key)
                                                         (funcall node-n-values tree)))
                      1
                      nil))

             (t
              (error "Unrecognized tree object: ~W" tree))))

          ((:remove nil)
           (values tree 0 nil))))))

  (define-specializable-function tree-mutate-equal (<interface>) (tree action-function extracted-key extracted-value)
    (with-interface <interface>
        (node-1-copy node-1-type-p node-n-copy node-n-type-p node-n-values map-p)
      (multiple-value-bind
            (action value)
          (if (funcall map-p)
              (funcall action-function extracted-key extracted-value)
              (funcall action-function extracted-key))

        (ecase action
          ((nil)
           (values tree 0 nil))

          (:remove
           (cond
             ((funcall node-1-type-p tree)
              (values (tree-append-children <interface> tree)
                      -1
                      t))
             ((funcall node-n-type-p tree)
              (values (node-n-copy-with-values <interface> tree (cdr (funcall node-n-values tree)))
                      -1
                      nil))
             (t
              (error "Invalid tree object: ~W" tree))))

          (:insert
           (if (funcall map-p)
               (cond
                 ((funcall node-1-type-p tree)
                  (values (funcall node-1-copy tree :value value)
                          0
                          nil))
                 ((funcall node-n-type-p tree)
                  (if (eql value extracted-value)
                      (values tree 0 nil)
                      (values (funcall node-n-copy tree :values (cons (cons extracted-key value)
                                                                      (cdr (funcall node-n-values tree))))
                              0
                              nil)))
                 (t
                  (error "Invalid tree object: ~W" tree)))
               (values tree 0 nil)))))))

  (define-specializable-function tree-mutate-unequal (<interface>) (tree comparator key action-function)
    (with-interface <interface>
        (node-1-type-p node-n-type-p node-n-values map-p)
      (cond
        ((funcall node-1-type-p tree)
         (tree-mutate-missing <interface> tree key action-function))

        ((funcall node-n-type-p tree)
         (multiple-value-bind
               (result count-change balance-needed-p)
             (if (funcall map-p)
                 ;; We don't need to check the first node -- we already know its unequal
                 (list-map-mutate comparator (cdr (funcall node-n-values tree)) key action-function)
                 (list-set-mutate comparator (cdr (funcall node-n-values tree)) key action-function))
           (cassert (null balance-needed-p))

           (when (eql result (cdr (funcall node-n-values tree)))
             (cassert (equal count-change 0))
             (return-from tree-mutate-unequal
               (values tree count-change balance-needed-p)))

           (values
            (node-n-copy-with-values <interface> tree
                                     (cons (car (funcall node-n-values tree))
                                           result))
            count-change
            balance-needed-p)))

        (t
         (error "Invalid tree object: ~W" tree)))))

  (define-specializable-function tree-mutate-less (<interface>) (tree comparator key action-function)
    (with-interface <interface>
        (node-left insert-left-balancer remove-left-balancer)
      (multiple-value-bind
            (result count-change balance-needed-p)
          (tree-mutate <interface> (funcall node-left tree) comparator key action-function)
        (values
         (if balance-needed-p
             (if (plusp count-change)
                 (funcall insert-left-balancer tree :left result)
                 (funcall remove-left-balancer tree :left result))
             (node-copy <interface> tree :left result))
         count-change
         balance-needed-p))))

  (define-specializable-function tree-mutate-greater (<interface>) (tree comparator key action-function)
    (with-interface <interface>
        (node-right insert-right-balancer remove-right-balancer)
      (multiple-value-bind
            (result count-change balance-needed-p)
          (tree-mutate <interface> (funcall node-right tree) comparator key action-function)
        (values
         (if balance-needed-p
             (if (plusp count-change)
                 (funcall insert-right-balancer tree :right result)
                 (funcall remove-right-balancer tree :right result))
             (node-copy <interface> tree :right result))
         count-change
         balance-needed-p))))

  (define-specializable-function tree-mutate (<interface>) (tree comparator key action-function)
    (with-interface <interface>
        (nil-type-p node-type-p)
      (cond
        ((funcall nil-type-p tree)
         (tree-mutate-missing <interface> tree key action-function))

        ((funcall node-type-p tree)
         (multiple-value-bind (extracted-key extracted-value) (representative-key <interface> tree)
           (let ((comparison (funcall comparator key extracted-key)))
             (ecase comparison
               (:less
                (tree-mutate-less <interface> tree comparator key action-function))
               (:greater
                (tree-mutate-greater <interface> tree comparator key action-function))
               (:unequal
                (tree-mutate-unequal <interface> tree comparator key action-function))
               (:equal
                (tree-mutate-equal <interface> tree action-function extracted-key extracted-value))))))

        (t
         (error "Invalid tree object: ~W" tree))))))

(define-specializable-function tree-insert (<interface>) (tree comparator key value)
  (with-interface <interface>
      (map-p)
    (tree-mutate
     <interface>
     tree
     comparator
     key
     (if (funcall map-p)
         (lambda (&optional extracted-key extracted-value)
           (declare (ignore extracted-key extracted-value))
           (values :insert value))
         (lambda (&optional extracted-key)
           (declare (ignore extracted-key))
           :insert)))))

(define-specializable-function tree-emplace (<interface>) (tree comparator key value)
  (with-interface <interface>
      (map-p)
    (tree-mutate
     <interface>
     tree
     comparator
     key
     (if (funcall map-p)
         (lambda (&optional (extracted-key nil value-p) extracted-value)
           (declare (ignore extracted-key extracted-value))
           (unless value-p
             (values :insert value)))
         (lambda (&optional extracted-key)
           (declare (ignore extracted-key))
           :insert)))))

(define-specializable-function make-map-tree (<interface>) (comparator &key alist plist)
  (with-interface <interface>
      (make-nil map-p)
    (unless (funcall map-p)
      (error "make-map-tree only works for maps"))

    (let ((tree (funcall make-nil))
          (count 0))
      (labels
          ((add (key value)
             (multiple-value-bind
                      (result count-change balance-needed-p)
                    (tree-emplace <interface> tree comparator key value)
                  (declare (ignore balance-needed-p))
                  (setf tree result)
                  (incf count count-change))))
        (loop :with remaining-plist = plist :while remaining-plist
              :for key = (pop remaining-plist)
              :for value = (if remaining-plist
                               (pop remaining-plist)
                               (error "Odd number of items in plist"))
              :do (add key value))
        (loop :for pair :in alist
              :for key = (car pair)
              :for value = (cdr pair)
              :do (add key value)))
      (values tree count))))

(define-specializable-function make-set-tree (<interface>) (comparator &key items)
  (with-interface <interface>
      (make-nil map-p)
    (when (funcall map-p)
      (error "make-set-tree only works for sets"))

    (let ((tree (funcall make-nil))
          (count 0))
      (labels
          ((add (key)
             (multiple-value-bind
                      (result count-change balance-needed-p)
                    (tree-emplace <interface> tree comparator key t)
                  (declare (ignore balance-needed-p))
                  (setf tree result)
                  (incf count count-change))))
        (loop :for key :in items
              :do (add key)))
      (values tree count))))

(define-specializable-function tree-lookup (<interface>) (tree comparator key)
  ;; Is it actually faster to have a redundant copy of the
  ;; traversal logic that can't mutate rather than using the
  ;; mutator function w/ a mutator action that does a
  ;; non-local return to quickly unwind the stack?  Maaaaybe.
  ;; Hypothetically, the less and greater paths below could
  ;; be TCO'd out and we could avoid introducing stack frames
  ;; as we traverse.
  (with-interface <interface>
      (nil-type-p node-type-p node-1-type-p node-left node-n-values node-right
                  map-p)
    (cond
      ((funcall nil-type-p tree)
       (values nil nil))
      ((funcall node-type-p tree)
       (multiple-value-bind (here-key here-value) (representative-key <interface> tree)
         (ecase (funcall comparator key here-key)
           (:less
            (tree-lookup <interface> (funcall node-left tree) comparator key))
           (:greater
            (tree-lookup <interface> (funcall node-right tree) comparator key))
           (:equal
            (values here-value t))
           (:unequal
            (if (funcall node-1-type-p tree)
                (values nil nil)
                (if (funcall map-p)
                    ;; We have already looked at the first element!
                    (list-map-lookup comparator (cdr (funcall node-n-values tree)) key)
                    (let ((member-p (list-set-is-member comparator (cdr (funcall node-n-values tree)) key)))
                      (values member-p member-p)))))))))))

(define-specializable-function tree-remove (<interface>) (tree comparator key)
  (with-interface <interface>
      (map-p)
    (let (value value-p)
      (multiple-value-bind
            (new-tree count-change)
          (tree-mutate <interface> tree comparator key
                       (lambda (&optional (extracted-key nil found-p)
                                  (extracted-value (if (funcall map-p) nil t)))
                         (declare (ignore extracted-key))
                         (setf value extracted-value)
                         (setf value-p found-p)
                         :remove))
        (cassert (or (and (zerop count-change) (null value-p))
                     (and (equal -1 count-change) (not (null value-p)))))
        (values new-tree value value-p count-change)))))

(define-specializable-function tree-decompose (<interface>) (tree)
  (with-interface <interface>
      (nil-type-p node-left remove-left-balancer map-p)
    (when (funcall nil-type-p tree)
      (return-from tree-decompose (values tree nil nil)))

    (labels
        ((visit (tree)
           (unless (funcall nil-type-p (funcall node-left tree))
             (multiple-value-bind
                   (new-tree value valid-p balance-needed-p)
                 (visit (funcall node-left tree))
               (return-from visit
                 (values (if balance-needed-p
                             (funcall remove-left-balancer tree :left new-tree)
                             (node-copy <interface> tree :left new-tree))
                         value
                         valid-p
                         balance-needed-p))))

           (let (extracted-key
                 extracted-value)
             (multiple-value-bind
                   (new-tree count-change balance-needed-p)
                 (tree-mutate <interface> tree (constantly :equal) nil
                         (lambda (key &optional (value t))
                           (setf extracted-key key)
                           (setf extracted-value value)
                           :remove))
               (assert (equal -1 count-change))
               (values new-tree
                       (if (funcall map-p)
                           (cons extracted-key extracted-value)
                           extracted-key)
                       t
                       balance-needed-p)))))
      (visit tree))))

(define-specializable-function tree-for-each (<interface>) (tree function)
  (with-interface <interface>
      (nil-type-p node-left n-type-values node-1-key
                  node-1-type-p node-1-value node-n-type-p node-right
                  map-p)
    (when (funcall nil-type-p tree)
      (return-from tree-for-each))

    (tree-for-each <interface> (funcall node-left tree) function)
    (cond
      ((funcall node-1-type-p tree)
       (if (funcall map-p)
           (funcall function (cons (funcall node-1-key tree) (funcall node-1-value tree)))
           (funcall function (funcall node-1-key tree))))
      ((funcall node-n-type-p tree)
       (dolist (value (funcall n-type-values tree))
         (if (funcall map-p)
             (funcall function (cons (car value) (cdr value)))
             (funcall function value)))))
    (tree-for-each <interface> (funcall node-right tree) function)))

(define-specializable-function tree-to-list (<interface>) (tree)
  (let ((builder (make-impure-list-builder)))
    (tree-for-each
     <interface> tree
     (lambda (object) (impure-list-builder-add builder object)))
    (impure-list-builder-extract builder)))

(define-specializable-function tree-map-members (<interface>) (tree comparator function)
  (with-interface <interface>
      (map-p)
    (let ((builder (make-impure-list-builder)))
      (tree-for-each
       <interface> tree
       (lambda (item)
         (impure-list-builder-add builder (funcall function item))))
      (if (funcall map-p)
          (make-map-tree <interface> comparator :alist (impure-list-builder-extract builder))
          (make-set-tree <interface> comparator :items (impure-list-builder-extract builder))))))

(define-specializable-function tree-map-entries (<interface>) (tree function)
  (with-interface <interface>
      (nil-type-p node-1-type-p node-1-copy node-n-type-p node-n-copy node-left node-right
                  node-1-key node-1-value node-n-values map-p)
    (unless (funcall map-p)
      (error "TREE-MAP-ENTRIES only works on maps"))

    (cond
      ((funcall nil-type-p tree)
       tree)

      ((funcall node-1-type-p tree)
       (funcall node-1-copy tree
                :left (tree-map-entries <interface> (funcall node-left tree) function)
                :right (tree-map-entries <interface> (funcall node-right tree) function)
                :value (funcall function
                                (funcall node-1-key tree)
                                (funcall node-1-value tree))))

      ((funcall node-n-type-p tree)
       (funcall node-n-copy tree
                :left (tree-map-entries <interface> (funcall node-left tree) function)
                :right (tree-map-entries <interface> (funcall node-right tree) function)
                :values (list-map-map-entries (funcall node-n-values tree) function))))))

(define-specializable-function tree-node-value-iterator (<interface>) (tree)
  (with-interface <interface>
      (node-1-type-p node-n-type-p node-1-key node-1-value node-n-values
                     map-p)
    (cond
      ((funcall node-1-type-p tree)
       (singleton-iterator
        (if (funcall map-p)
            (cons (funcall node-1-key tree) (funcall node-1-value tree))
            (funcall node-1-key tree))))

      ((funcall node-n-type-p tree)
       (list-iterator (funcall node-n-values tree))))))

(define-specializable-function tree-iterator (<interface>) (tree)
  (with-interface <interface>
      (nil-type-p node-left node-right)
    (when (funcall nil-type-p tree)
      (return-from tree-iterator
        (empty-iterator)))

    (let (stack)
      (labels
          ((push-node (node)
             (unless (funcall nil-type-p node)
               (push (list (funcall node-left node)
                           (tree-node-value-iterator <interface> node)
                           (funcall node-right node))
                     stack))))
        (push-node tree)
        (lambda ()
          (loop
            (unless stack
              (return (values nil nil)))

            (let ((tip (car stack)))
              (destructuring-bind (tip-left tip-node-iter tip-right) tip
                (cond
                  (tip-left
                   (push-node tip-left)
                   (setf (first tip) nil))

                  (tip-node-iter
                   (multiple-value-bind (value valid-p) (funcall tip-node-iter)
                     (if valid-p
                         (return (values value valid-p))
                         (setf (second tip) nil))))

                  (t
                   (assert tip-right)
                   ;; Instead of setting this element to nil, let's
                   ;; just drop the whole frame.  We don't need it
                   ;; any more!
                   (pop stack)
                   (push-node tip-right)))))))))))

(define-specializable-function tree-print-graphviz (<interface>) (tree stream id-vendor)
  (with-interface <interface>
      (nil-type-p node-1-type-p node-n-type-p
                  node-1-key node-1-value node-n-values
                  prepare-graphviz-properties node-left node-right
                  map-p)
    (let ((id (next-graphviz-id id-vendor))
          (properties (make-hash-table :test #'equal)))

      (format stream "ID~A [" id)

      (setf (gethash "shape" properties) "box")
      (cond
        ((funcall nil-type-p tree)
         (setf (gethash "label" properties) nil)
         (setf (gethash "shape" properties) "diamond"))

        ((funcall node-1-type-p tree)
         (setf (gethash "label" properties)
                   (if (funcall map-p)
                       (cons (funcall node-1-key tree) (funcall node-1-value tree))
                       (funcall node-1-key tree))))

        ((funcall node-n-type-p tree)
         (setf (gethash "label" properties) (funcall node-n-values tree))))

      (funcall prepare-graphviz-properties tree properties)

      (loop :for key :being :the :hash-keys :of properties
            :using (:hash-value value) :do
            (format stream " ~A=~A" (graphviz-quote key) (graphviz-quote value)))

      (format stream " ]~%")

      (unless (funcall nil-type-p tree)
        (let ((child-id (tree-print-graphviz <interface> (funcall node-left tree) stream id-vendor)))
          (format stream "ID~A -> ID~A~%" id child-id))
        (let ((child-id (tree-print-graphviz <interface> (funcall node-right tree) stream id-vendor)))
          (format stream "ID~A -> ID~A~%" id child-id)))
      id)))

(define-specializable-function check-tree-order (<interface>) (tree comparator)
  (with-interface <interface>
      (nil-type-p node-left node-right representative-key)
    (when (funcall nil-type-p tree)
      (return-from check-tree-order))

    (let ((left (funcall node-left tree))
          (right (funcall node-right tree))
          (here-key (funcall representative-key tree)))
      (unless (funcall nil-type-p left)
        (cassert (eq :less (funcall comparator
                                    (funcall representative-key left)
                                    here-key))
                 nil "Left child must be less than its parent"))
      (unless (funcall nil-type-p right)
        (cassert (eq :greater (funcall comparator
                                    (funcall representative-key right)
                                    here-key))
                 nil "Right child must be less than its parent"))

      (check-tree-order <interface> left comparator)
      (check-tree-order <interface> right comparator))))

(define-specializable-function check-tree-count (<interface>) (tree)
  (with-interface <interface>
      (nil-type-p node-1-type-p node-n-type-p node-left node-right
                  node-n-values)
    (when (funcall nil-type-p tree)
      (return-from check-tree-count 0))

    (let* ((left (funcall node-left tree))
           (left-count (check-tree-count <interface> left))
           (right (funcall node-right tree))
           (right-count (check-tree-count <interface> right))
           (children-count (+ left-count right-count)))
      (cond
        ((funcall node-1-type-p tree)
         (+ 1 children-count))

        ((funcall node-n-type-p tree)
         (cassert (cdr (funcall node-n-values tree))
                  nil "n-type nodes must have multiple values")
         (+ (length (funcall node-n-values tree))
            children-count))))))
