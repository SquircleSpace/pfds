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

(defpackage :pfds.shcl.io/utility/tree
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
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

(defgeneric print-tree-node-properties (object stream))

(defgeneric nil-tree-p (tree))
(defgeneric node-left (node))
(defgeneric node-right (node))
(defgeneric node-values (node))

(defmacro define-tree (base-name (&key
                                    (map-p t)

                                    (define-mutate-p t)
                                    (mutate (when define-mutate-p (intern-conc *package* base-name "-MUTATE")))
                                    (define-mutate-less-p define-mutate-p)
                                    (mutate-less (when define-mutate-less-p (intern-conc *package* base-name "-MUTATE-LESS")))
                                    (define-mutate-greater-p define-mutate-p)
                                    (mutate-greater (when define-mutate-greater-p (intern-conc *package* base-name "-MUTATE-GREATER")))
                                    (define-mutate-equal-p define-mutate-p)
                                    (mutate-equal (when define-mutate-equal-p (intern-conc *package* base-name "-MUTATE-EQUAL")))
                                    (define-mutate-unequal-p define-mutate-p)
                                    (mutate-unequal (when define-mutate-unequal-p (intern-conc *package* base-name "-MUTATE-UNEQUAL")))
                                    (define-mutate-missing-p define-mutate-p)
                                    (mutate-missing (when define-mutate-missing-p (intern-conc *package* base-name "-MUTATE-MISSING")))

                                    (define-lookup-p t)
                                    (lookup (when define-lookup-p (intern-conc *package* base-name "-LOOKUP")))

                                    (define-emplace-p (and map-p mutate))
                                    (emplace (when define-emplace-p (intern-conc *package* base-name "-EMPLACE")))
                                    (define-insert-p mutate)
                                    (insert (when define-insert-p (intern-conc *package* base-name "-INSERT")))
                                    (define-remove-p mutate)
                                    (remove (when define-remove-p (intern-conc *package* base-name "-REMOVE")))
                                    (define-decompose-p mutate)
                                    (decompose (when define-decompose-p (intern-conc *package* base-name "-DECOMPOSE")))

                                    (define-maker-p (if map-p emplace insert))
                                    (maker (when define-maker-p (intern-conc *package* "MAKE-" base-name)))

                                    (define-checker-p t)
                                    (checker (when define-checker-p (intern-conc *package* "CHECK-" base-name)))

                                    (define-map-members-p maker)
                                    (map-members (when define-map-members-p (intern-conc *package* base-name "-MAP-MEMBERS")))

                                    (define-map-entries-p map-p)
                                    (map-entries (when define-map-entries-p (intern-conc *package* base-name "-MAP-ENTRIES")))

                                    (define-iterator-p t)
                                    (iterator (when define-iterator-p (intern-conc *package* base-name "-ITERATOR")))

                                    (define-append-children-p t)
                                    (append-children (when define-append-children-p (intern-conc *package* base-name "-APPEND-TREES")))

                                    (define-do-tree-f t)
                                    (do-tree-f (when define-do-tree-f (intern-conc *package* "DO-" base-name "-F")))

                                    insert-left-balancer insert-right-balancer
                                    remove-left-balancer remove-right-balancer)
                       &body extra-node-slots)
  (let* ((nil-type (intern-conc *package* base-name "-NIL"))
         (nil-type-p (intern-conc *package* nil-type "-P"))
         (nil-var (intern-conc *package* "*" nil-type "*"))
         (node-base-type (intern-conc *package* base-name "-NODE"))
         (node-1-type (intern-conc *package* base-name "-NODE-1"))
         (node-n-type (intern-conc *package* base-name "-NODE-N"))
         (make-nil-type (intern-conc *package* "MAKE-" nil-type))
         (make-1-type (intern-conc *package* "MAKE-" node-1-type))
         (copy-n-type (intern-conc *package* "COPY-" node-n-type))
         (copy-1-type (intern-conc *package* "COPY-" node-1-type))
         (n-type-values (intern-conc *package* node-n-type "-VALUES"))
         (1-type-key (intern-conc *package* node-1-type "-KEY"))
         (1-type-value (intern-conc *package* node-1-type "-VALUE"))
         (node-left (intern-conc *package* node-base-type "-LEFT"))
         (node-right (intern-conc *package* node-base-type "-RIGHT"))
         (to-list (intern-conc *package* base-name "-TO-LIST"))
         (node-copy (intern-conc *package* "COPY-" node-base-type))
         (do-tree (when do-tree-f (intern-conc *package* "DO-" base-name)))
         (representative-key (intern-conc *package* node-base-type "-REPRESENTATIVE"))
         (remove-min-node (gensym "REMOVE-MIN-NODE"))
         (balance-needed-p (gensym "BALANCE-NEEDED-P"))
         (values (gensym "VALUES"))
         (result (gensym "RESULT"))
         (comparator (gensym "COMPARATOR"))
         (comparison (gensym "COMPARISON"))
         (tree (gensym "TREE"))
         (new-tree (gensym "NEW-TREE"))
         (stream (gensym "STREAM"))
         (other (gensym "OTHER"))
         (sublist (gensym "SUBLIST"))
         (key (gensym "KEY"))
         (value (gensym "VALUE"))
         (value-p (gensym "VALUE-P"))
         (copy-n-type-with-values (gensym "COPY-N-TYPE-WITH-VALUES"))
         (count-change (gensym "COUNT-CHANGE"))
         (extracted-key (gensym "EXTRACTED-KEY"))
         (extracted-value (gensym "EXTRACTED-VALUE"))
         (action (gensym "ACTION"))
         (action-function (gensym "ACTION-FUNCTION"))
         (value-list (when map-p `(,value)))
         (impl (gensym "IMPL"))
         (min (gensym "MIN"))
         (without-min (gensym "WITHOUT-MIN"))
         (function (gensym "FUNCTION"))
         (body (gensym "BODY"))
         (fn (gensym "FN"))
         (builder (gensym "BUILDER"))
         (alist (make-symbol "ALIST"))
         (plist (make-symbol "PLIST"))
         (items (make-symbol "ITEMS"))
         (id-vendor (gensym "ID-VENDOR"))
         (count (gensym "COUNT"))
         (node-value-iterator (gensym "NODE-VALUE-ITERATOR"))
         (stack (gensym "STACK"))
         (tip (gensym "TIP"))
         (tip-left (gensym "TIP-LEFT"))
         (tip-right (gensym "TIP-RIGHT"))
         (tip-node-iter (gensym "TIP-NODE-ITER"))
         (valid-p (gensym "VALID-P")))

    (unless insert-left-balancer
      (setf insert-left-balancer node-copy))

    (unless insert-right-balancer
      (setf insert-right-balancer node-copy))

    (unless remove-left-balancer
      (setf remove-left-balancer node-copy))

    (unless remove-right-balancer
      (setf remove-right-balancer node-copy))

    `(progn
       (defvar ,nil-var)
       (defun ,nil-type ()
         ,nil-var)

       (define-adt ,base-name
           ()
         (,nil-type)

         ((,node-base-type (:copier nil)
                           (:constructor nil))
          (left (,nil-type) :type ,base-name)
          (right (,nil-type) :type ,base-name)
          ,@extra-node-slots))

       (define-immutable-structure (,node-1-type (:include ,node-base-type))
         (key (error "required"))
         ,@(when map-p
             `((value (error "required")))))

       (define-immutable-structure (,node-n-type (:include ,node-base-type))
         (values (error "required")))

       (defvar ,nil-var (,make-nil-type))

       (declaim (inline ,representative-key))
       (defun ,representative-key (,tree)
         (etypecase ,tree
           (,node-n-type
            (let ((,value (car (,n-type-values ,tree))))
              ,(if map-p
                   `(values (car ,value) (cdr ,value))
                   `(values ,value t))))
           (,node-1-type
            ,(if map-p
                 `(values (,1-type-key ,tree) (,1-type-value ,tree))
                 `(values (,1-type-key ,tree) t)))))

       (declaim (inline ,node-copy))
       (defun ,node-copy (,tree &rest ,values)
         (etypecase ,tree
           (,node-1-type
            (apply #',copy-1-type ,tree ,values))
           (,node-n-type
            (apply #',copy-n-type ,tree ,values))))

       ,@(when define-append-children-p
           `((defun ,append-children (,tree)
               (when (,nil-type-p (,node-left ,tree))
                 (return-from ,append-children (,node-right ,tree)))
               (when (,nil-type-p (,node-right ,tree))
                 (return-from ,append-children (,node-left ,tree)))

               (multiple-value-bind (,min ,without-min) (,remove-min-node (,node-right ,tree))
                 ;; Now we make a version of tree that has all the
                 ;; same metadata but the value(s) of min.  We're
                 ;; just migrating the min values up to the position
                 ;; of tree.  Afterwards, we'll run the balancer to
                 ;; update the metadata.
                 (setf ,tree (etypecase ,tree
                               (,node-1-type
                                (etypecase ,min
                                  (,node-1-type
                                   (,copy-1-type ,tree :key (,1-type-key ,min) ,@(when map-p `(:value (,1-type-value ,min)))))
                                  (,node-n-type
                                   (structure-convert (,node-n-type ,node-1-type) ,tree :values (,n-type-values ,min)))))
                               (,node-n-type
                                (etypecase ,min
                                  (,node-1-type
                                   (structure-convert (,node-1-type ,node-n-type) ,tree :key (,1-type-key ,min)
                                                      ,@(when map-p `(:value (,1-type-value ,min)))))
                                  (,node-n-type
                                   (,copy-n-type ,tree :values (,n-type-values ,min)))))))
                 (,remove-right-balancer ,tree :right ,without-min)))

             (defun ,remove-min-node (,tree)
               (etypecase ,tree
                 (,nil-type
                  (error "nil tree has no min"))
                 ((or ,node-1-type ,node-n-type)
                  (if (,nil-type-p (,node-left ,tree))
                      (values ,tree (,node-right ,tree))
                      (multiple-value-bind (,result ,value) (,remove-min-node (,node-left ,tree))
                        (values ,result (,remove-left-balancer ,tree :left ,value)))))))))

       ,@(when define-mutate-missing-p
           `((defun ,mutate-missing (,tree ,key ,action-function)
               (multiple-value-bind (,action ,@value-list) (funcall ,action-function)
                 (ecase ,action
                   (:insert
                    (etypecase ,tree
                      (,nil-type
                       (values (,make-1-type :key ,key ,@(when map-p `(:value ,value)))
                               1
                               t))
                      (,node-1-type
                       (values (structure-convert (,node-n-type ,node-1-type)
                                                  ,tree
                                                  :values ,(if map-p
                                                               `(list
                                                                 (cons (,1-type-key ,tree) (,1-type-value ,tree))
                                                                 (cons ,key ,value))
                                                               `(list
                                                                 (,1-type-key ,tree)
                                                                 ,key)))
                               1
                               nil))
                      (,node-n-type
                       (values (,copy-n-type ,tree :values (cons ,(if map-p
                                                                      `(cons ,key ,value)
                                                                      key)
                                                                 (,n-type-values ,tree)))
                               1
                               nil))))

                   ((:remove nil)
                    (values ,tree 0 nil)))))))

       ,@(when define-mutate-equal-p
           `((defun ,copy-n-type-with-values (,tree ,values)
               (check-type ,tree ,node-n-type)
               (if (cdr ,values)
                   (,copy-n-type ,tree :values ,values)
                   (structure-convert (,node-1-type ,node-n-type) ,tree
                                      :key ,(if map-p
                                                `(car (car ,values))
                                                `(car ,values))
                                      ,@(when map-p
                                          `(:value (cdr (car ,values)))))))

             (defun ,mutate-equal (,tree ,extracted-key ,@(when map-p `(,extracted-value)) ,action-function)
               (multiple-value-bind
                     (,action ,@value-list)
                   (funcall ,action-function ,extracted-key ,@(when map-p `(,extracted-value)))
                 (ecase ,action
                   ((nil)
                    (values ,tree 0 nil))
                   (:remove
                    (etypecase ,tree
                      (,node-1-type
                       (values (,append-children ,tree)
                               -1
                               t))
                      (,node-n-type
                       (values (,copy-n-type-with-values ,tree (cdr (,n-type-values ,tree)))
                               -1
                               nil))))
                   ,@(when map-p
                       `((:insert
                          (etypecase ,tree
                            (,node-1-type
                             (values (,copy-1-type ,tree :value ,value)
                                     0
                                     nil))
                            (,node-n-type
                             (values (,copy-n-type ,tree :values (cons (cons ,extracted-key ,value)
                                                                       (cdr (,n-type-values ,tree))))
                                     0
                                     nil)))))))))))

       ,@(when define-mutate-unequal-p
           (unless mutate-missing (error "Cannot define mutate-unequal without mutate-missing"))
           `((defun ,mutate-unequal (,comparator ,tree ,key ,action-function)
               (etypecase ,tree
                 (,node-1-type
                  (,mutate-missing ,tree ,key ,action-function))
                 (,node-n-type
                  (multiple-value-bind
                        (,result ,count-change ,balance-needed-p)
                      (,(if map-p 'list-map-mutate 'list-set-mutate)
                       ,comparator
                       ;; We don't need to check the first node -- we already know its unequal
                       (cdr (,n-type-values ,tree))
                       ,key
                       ,action-function)
                    (cassert (null ,balance-needed-p))

                    (when (eql ,result (cdr (,n-type-values ,tree)))
                      (cassert (equal ,count-change 0))
                      (return-from ,mutate-unequal
                        (values ,tree ,count-change ,balance-needed-p)))

                    (values
                     (,copy-n-type-with-values ,tree (cons (car (,n-type-values ,tree))
                                                           ,result))
                     ,count-change
                     ,balance-needed-p)))))))

       ,@(when define-mutate-less-p
           (unless mutate (error "Cannot define mutate-less without mutate"))
           `((defun ,mutate-less (,comparator ,tree ,key ,action-function)
               (multiple-value-bind
                     (,result ,count-change ,balance-needed-p)
                   (,mutate ,comparator (,node-left ,tree) ,key ,action-function)
                 (values
                  (if ,balance-needed-p
                      (if (plusp ,count-change)
                          (,insert-left-balancer ,tree :left ,result)
                          (,remove-left-balancer ,tree :left ,result))
                      (,node-copy ,tree :left ,result))
                  ,count-change
                  ,balance-needed-p)))))

       ,@(when define-mutate-greater-p
           (unless mutate (error "Cannot define mutate-less without mutate"))
           `((defun ,mutate-greater (,comparator ,tree ,key ,action-function)
               (multiple-value-bind
                     (,result ,count-change ,balance-needed-p)
                   (,mutate ,comparator (,node-right ,tree) ,key ,action-function)
                 (values
                  (if ,balance-needed-p
                      (if (plusp ,count-change)
                          (,insert-right-balancer ,tree :right ,result)
                          (,remove-right-balancer ,tree :right ,result))
                      (,node-copy ,tree :right ,result))
                  ,count-change
                  ,balance-needed-p)))))

       ,@(when define-mutate-p
           (unless append-children (error "Must have an append-children in order to define mutate"))
           (unless (and mutate-missing mutate-equal mutate-greater mutate-less mutate-unequal)
             (error "Must have all the other mutate functions in order to define mutate"))
           `((defun ,mutate (,comparator ,tree ,key ,action-function)
               (etypecase ,tree
                 (,nil-type
                  (,mutate-missing ,tree ,key ,action-function))

                 ((or ,node-1-type ,node-n-type)
                  (multiple-value-bind (,extracted-key ,@value-list) (,representative-key ,tree)
                    (let ((,comparison (funcall ,comparator ,key ,extracted-key)))
                      (ecase ,comparison
                        (:less
                         (,mutate-less ,comparator ,tree ,key ,action-function))
                        (:greater
                         (,mutate-greater ,comparator ,tree ,key ,action-function))
                        (:unequal
                         (,mutate-unequal ,comparator ,tree ,key ,action-function))
                        (:equal
                         (,mutate-equal ,tree ,extracted-key ,@value-list ,action-function))))))))))

       ,@(when define-insert-p
           (unless mutate (error "must have a mutate function to define insert"))
           (if map-p
               `((defun ,insert (,comparator ,tree ,key ,value)
                   (,mutate ,comparator ,tree ,key (lambda (&optional ,extracted-key ,extracted-value)
                                                     (declare (ignore ,extracted-key ,extracted-value))
                                                     (values :insert ,value)))))
               `((defun ,insert (,comparator ,tree ,key)
                   (,mutate ,comparator ,tree ,key (lambda (&optional (,value nil ,value-p))
                                                     (declare (ignore ,value))
                                                     (unless ,value-p
                                                       :insert)))))))

       ,@(when define-emplace-p
           (unless map-p (error "Must be a map to define emplace"))
           (unless mutate (error "Must have mutate function to define emplace"))
           `((defun ,emplace (,comparator ,tree ,key ,value)
               (,mutate ,comparator ,tree ,key (lambda (&optional (,extracted-key nil ,value-p)
                                                          ,extracted-value)
                                                 (declare (ignore ,extracted-key ,extracted-value))
                                                 (unless ,value-p
                                                   (values :insert ,value)))))))

       ,@(when define-maker-p
           (if map-p
               `((defun ,maker (,comparator &key ,alist ,plist)
                   (let ((,tree (,nil-type))
                         (,count 0))
                     (loop :while ,plist
                           :for ,key = (pop ,plist)
                           :for ,value = (if ,plist (pop ,plist) (error "Odd number of items in plist"))
                           :do (multiple-value-bind
                                     (,result ,count-change ,balance-needed-p)
                                   (,emplace ,comparator ,tree ,key ,value)
                                 (declare (ignore ,balance-needed-p))
                                 (setf ,tree ,result)
                                 (incf ,count ,count-change)))
                     (dolist (,value ,alist)
                       (multiple-value-bind
                             (,result ,count-change ,balance-needed-p)
                           (,emplace ,comparator ,tree (car ,value) (cdr ,value))
                         (declare (ignore ,balance-needed-p))
                         (setf ,tree ,result)
                         (incf ,count ,count-change)))
                     (values ,tree ,count))))
               `((defun ,maker (,comparator &key ,items)
                   (let ((,tree (,nil-type))
                         (,count 0))
                     (dolist (,value ,items)
                       (multiple-value-bind
                             (,result ,count-change ,balance-needed-p)
                           (,insert ,comparator ,tree ,value)
                         (declare (ignore ,balance-needed-p))
                         (setf ,tree ,result)
                         (incf ,count ,count-change)))
                     (values ,tree ,count))))))

       ,@(when define-lookup-p
           ;; Is it actually faster to have a redundant copy of the
           ;; traversal logic that can't mutate rather than using the
           ;; mutator function w/ a mutator action that does a
           ;; non-local return to quickly unwind the stack?  Maaaaybe.
           ;; Hypothetically, the less and greater paths below could
           ;; be TCO'd out and we could avoid introducing stack frames
           ;; as we traverse.
           `((defun ,lookup (,comparator ,tree ,key)
               (etypecase ,tree
                 (,nil-type
                  (values nil nil))
                 ((or ,node-1-type ,node-n-type)
                  (multiple-value-bind (,result ,value) (,representative-key ,tree)
                    (ecase (funcall ,comparator ,key ,result)
                      (:less
                       (,lookup ,comparator (,node-left ,tree) ,key))
                      (:greater
                       (,lookup ,comparator (,node-right ,tree) ,key))
                      (:equal
                       (values ,value t))
                      (:unequal
                       (etypecase ,tree
                         (,node-1-type
                          (values nil nil))
                         (,node-n-type
                          ,(if map-p
                               `(list-map-lookup ,comparator (,n-type-values ,tree) ,key)
                               `(let ((,result (list-set-is-member ,comparator (,n-type-values ,tree) ,key)))
                                  (values ,result ,result)))))))))))))

       ,@(when define-remove-p
           (unless mutate (error "remove requires a mutate function"))
           `((defun ,remove (,comparator ,tree ,key)
               (let (,value
                     ,value-p)
                 (multiple-value-bind
                       (,new-tree ,count-change)
                     (,mutate ,comparator ,tree ,key
                              (lambda (&optional (,extracted-key nil ,valid-p) (,extracted-value ,(if map-p nil t)))
                                (declare (ignore ,extracted-key))
                                (setf ,value ,extracted-value)
                                (setf ,value-p ,valid-p)
                                :remove))
                   (cassert (or (and (zerop ,count-change) (null ,value-p))
                                (and (equal -1 ,count-change) (not (null ,value-p)))))
                   (values ,new-tree ,value ,value-p ,count-change))))))

       ,@(when define-decompose-p
           (unless mutate (error "Cannot define decompose without mutate"))
           `((defun ,decompose (,tree)
               (when (,nil-type-p ,tree)
                 (return-from ,decompose (values ,tree nil nil)))

               (labels
                   ((,impl (,tree)
                      (unless (,nil-type-p (,node-left ,tree))
                        (multiple-value-bind (,new-tree ,value ,valid-p ,balance-needed-p) (,impl (,node-left ,tree))
                          (return-from ,impl
                            (values (if ,balance-needed-p
                                        (,remove-left-balancer ,tree :left ,new-tree)
                                        (,node-copy ,tree :left ,new-tree))
                                    ,value
                                    ,valid-p
                                    ,balance-needed-p))))

                      (let (,extracted-key
                            ,@(when map-p `(,extracted-value)))
                        (multiple-value-bind
                              (,new-tree ,count-change ,balance-needed-p)
                            (,mutate (constantly :equal) ,tree nil
                                     (lambda (,key ,@value-list)
                                       (setf ,extracted-key ,key)
                                       ,@(when map-p `((setf ,extracted-value ,value)))
                                       :remove))
                          (assert (equal -1 ,count-change))
                          (values ,new-tree ,(if map-p `(cons ,extracted-key ,extracted-value) extracted-key) t ,balance-needed-p)))))
                 (,impl ,tree)))))

       ,@(when define-do-tree-f
           `((defun ,do-tree-f (,tree ,fn)
               (when (,nil-type-p ,tree)
                 (return-from ,do-tree-f))

               (,do-tree-f (,node-left ,tree) ,fn)
               (etypecase ,tree
                 (,node-1-type
                  (funcall ,fn (,1-type-key ,tree) ,@(when map-p `((,1-type-value ,tree)))))
                 (,node-n-type
                  (dolist (,value (,n-type-values ,tree))
                    ,(if map-p
                         `(funcall ,fn (car ,value) (cdr ,value))
                         `(funcall ,fn ,value)))))
               (,do-tree-f (,node-right ,tree) ,fn))))

       ,@(when do-tree-f
           `((defmacro ,do-tree ((,key ,@value-list ,tree &optional ,result) &body ,body)
               `(block nil
                  (,',do-tree-f ,,tree (lambda (,,key ,,@value-list) ,@,body))
                  ,,result))

             (defun ,to-list (,tree)
               (let ((,builder (make-impure-list-builder)))
                 (,do-tree (,key ,@value-list ,tree)
                   (impure-list-builder-add
                    ,builder
                    ,(if map-p
                         `(cons ,key ,value)
                         key)))
                 (impure-list-builder-extract ,builder)))

             (defmethod to-list ((,tree ,base-name))
               (,to-list ,tree))

             (defmethod for-each ((,tree ,base-name) ,function)
               (,do-tree (,key ,@value-list ,tree)
                 (funcall ,function ,key ,@value-list)))))

       ,@(when define-map-members-p
           (unless maker (error "Cannot define map-members without a maker function"))
           (unless do-tree (error "Cannot define map-members without a do-tree-f"))
           (if map-p
               `((defun ,map-members (,comparator ,tree ,function)
                   (let ((,builder (make-impure-list-builder)))
                     (,do-tree (,key ,value ,tree)
                       (impure-list-builder-add ,builder (funcall ,function (cons ,key ,value))))
                     (,maker ,comparator :alist (impure-list-builder-extract ,builder)))))
               `((defun ,map-members (,comparator ,tree ,function)
                   (let ((,builder (make-impure-list-builder)))
                     (,do-tree (,key ,tree)
                       (impure-list-builder-add ,builder (funcall ,function ,key)))
                     (,maker ,comparator :items (impure-list-builder-extract ,builder)))))))

       ,@(when define-map-entries-p
           (unless map-p (error "Cannot define map-entries for non-maps"))
           `((defun ,map-entries (,tree ,function)
               (etypecase ,tree
                 (,nil-type
                  ,tree)
                 (,node-1-type
                  (,copy-1-type ,tree
                                :left (,map-entries (,node-left ,tree) ,function)
                                :right (,map-entries (,node-right ,tree) ,function)
                                :value (funcall ,function (,1-type-key ,tree) (,1-type-value ,tree))))
                 (,node-n-type
                  (,copy-n-type ,tree
                                :left (,map-entries (,node-left ,tree) ,function)
                                :right (,map-entries (,node-right ,tree) ,function)
                                :values (list-map-map-entries (,n-type-values ,tree) ,function)))))))

       ,@(when define-iterator-p
           `((defun ,node-value-iterator (,tree)
               (etypecase ,tree
                 (,node-1-type
                  (lambda ()
                    (if ,tree
                        (multiple-value-prog1
                            (values ,(if map-p
                                         `(cons (,1-type-key ,tree) (,1-type-value ,tree))
                                         `(,1-type-key ,tree))
                                    t)
                          (setf ,tree nil))
                        (values nil nil))))
                 (,node-n-type
                  (let ((,value (,n-type-values ,tree)))
                    (lambda ()
                      (if ,value
                          (values (pop ,value) t)
                          (values nil nil)))))))

             (defun ,iterator (,tree)
               (when (,nil-type-p ,tree)
                 (return-from ,iterator
                   (lambda ()
                     (values nil nil))))

               (let ((,stack (list (list (,node-left ,tree)
                                         (,node-value-iterator ,tree)
                                         (,node-right ,tree)))))

                 (lambda ()
                   (loop
                     (unless ,stack
                       (return (values nil nil)))

                     (let ((,tip (car ,stack)))
                       (destructuring-bind (,tip-left ,tip-node-iter ,tip-right) ,tip
                         (cond
                           (,tip-left
                            (setf (first ,tip) nil)
                            (unless (,nil-type-p ,tip-left)
                              (push (list (,node-left ,tip-left)
                                          (,node-value-iterator ,tip-left)
                                          (,node-right ,tip-left))
                                    ,stack)))

                           (,tip-node-iter
                            (multiple-value-bind (,value ,valid-p) (funcall ,tip-node-iter)
                              (if ,valid-p
                                  (return (values ,value ,valid-p))
                                  (setf (second ,tip) nil))))

                           (t
                            (assert ,tip-right)
                            ;; Instead of setting this element to nil, let's
                            ;; just drop the whole frame.  We don't need it
                            ;; any more!
                            (pop ,stack)
                            (unless (,nil-type-p ,tip-right)
                              (push (list (,node-left ,tip-right)
                                          (,node-value-iterator ,tip-right)
                                          (,node-right ,tip-right))
                                    ,stack))))))))))

             (defmethod iterator ((,tree ,base-name))
               (,iterator ,tree))))

       (defmethod print-tree-node-properties ((,tree ,base-name) ,stream)
         (format ,stream "label=\"~A\" shape=box"
                 (unless (,nil-type-p ,tree)
                   (etypecase ,tree
                     (,node-1-type
                      ,(if map-p
                           `(cons (,1-type-key ,tree) (,1-type-value ,tree))
                           `(,1-type-key ,tree)))
                     (,node-n-type
                      (,n-type-values ,tree))))))

       (defmethod print-graphviz ((,tree ,base-name) ,stream ,id-vendor)
         (let ((,value (next-graphviz-id ,id-vendor)))
           (format ,stream "ID~A [" ,value)
           (print-tree-node-properties ,tree ,stream)
           (format ,stream "]~%")
           (unless (,nil-type-p ,tree)
             (let ((,result (print-graphviz (,node-left ,tree) ,stream ,id-vendor)))
               (format ,stream "ID~A -> ID~A~%" ,value ,result))
             (let ((,result (print-graphviz (,node-right ,tree) ,stream ,id-vendor)))
               (format ,stream "ID~A -> ID~A~%" ,value ,result)))
           ,value))

       (defmethod nil-tree-p ((,tree ,nil-type))
         t)

       (defmethod nil-tree-p ((,tree ,node-base-type))
         nil)

       (defmethod node-left ((,tree ,node-base-type))
         (,node-left ,tree))

       (defmethod node-right ((,tree ,node-base-type))
         (,node-right ,tree))

       (defmethod node-values ((,tree ,node-1-type))
         (list
          ,(if map-p
               `(cons (,1-type-key ,tree) (,1-type-value ,tree))
               `(,1-type-key ,tree))))

       (defmethod node-values ((,tree ,node-n-type))
         (,n-type-values ,tree))

       ,@(when define-checker-p
           `((defun ,checker (,tree ,comparator)
               (when (,nil-type-p ,tree)
                 (return-from ,checker 0))

               (unless (,nil-type-p (,node-left ,tree))
                 (cassert (eq :less (funcall ,comparator (,representative-key (,node-left ,tree)) (,representative-key ,tree)))
                          nil "Left child must be less than its parent"))

               (unless (,nil-type-p (,node-right ,tree))
                 (cassert (eq :greater (funcall ,comparator (,representative-key (,node-right ,tree)) (,representative-key ,tree)))
                          nil "Right child must be greater than its parent"))

               (let ((,count 0))
                 (incf ,count (,checker (,node-left ,tree) ,comparator))
                 (incf ,count (,checker (,node-right ,tree) ,comparator))
                 (etypecase ,tree
                   (,node-1-type
                    (+ ,count 1))
                   (,node-n-type
                    (cassert (cdr (,n-type-values ,tree))
                             nil "n-type nodes must have multiple values")
                    (loop :for ,sublist :on (,n-type-values ,tree)
                          :for ,key = ,(if map-p `(car (car ,sublist)) `(car ,sublist)) :do
                            (loop :for ,other :in (cdr ,sublist) :do
                              (cassert (eq :unequal (funcall ,comparator ,key ,(if map-p `(car ,other) other)))
                                       nil "Peer values in n-type must be mutually :unequal")))
                    (+ ,count (length (,n-type-values ,tree)))))))))

       ',base-name)))
