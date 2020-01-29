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

(defpackage :pfds.shcl.io/tree
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common #:to-list)
  (:import-from :pfds.shcl.io/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/immutable-structure
   #:define-adt #:structure-convert #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility
   #:intern-conc #:cassert)
  (:import-from :pfds.shcl.io/list-utility
   #:list-set-with #:list-set-without #:list-set #:list-set-is-member
   #:list-map-with #:list-map-without #:list-map #:list-map-lookup)
  (:export
   #:define-tree #:print-graphviz #:print-node-properties #:graphviz
   #:nil-tree-p #:node-left #:node-right #:node-values))
(in-package :pfds.shcl.io/tree)

(defvar *graph-id* 0)
(defgeneric print-node-properties (tree stream))
(defgeneric print-graphviz (tree stream))

(defun graphviz (tree &optional (stream *standard-output*))
  (let ((*graph-id* 0))
    (format stream "digraph {~%")
    (print-graphviz tree stream)
    (format stream "}~%"))
  (values))

(defgeneric nil-tree-p (tree))
(defgeneric node-left (node))
(defgeneric node-right (node))
(defgeneric node-values (node))

(defmacro define-tree (base-name (&key
                                    (map-p t)
                                    (define-lookup-p t)
                                    (define-insert-p t)
                                    (define-remove-p t)
                                    (define-maker-p t)
                                    (define-representative-p t)
                                    insert-left-balancer insert-right-balancer
                                    remove-left-balancer remove-right-balancer)
                       &body extra-node-slots)
  (let* ((maker (intern-conc *package* "MAKE-" base-name))
         (nil-type (intern-conc *package* base-name "-NIL"))
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
         (do-tree (intern-conc *package* "DO-" base-name))
         (do-tree-f (gensym "DO-TREE"))
         (insert (when define-insert-p (intern-conc *package* base-name "-INSERT")))
         (lookup (when define-lookup-p (intern-conc *package* base-name "-LOOKUP")))
         (remove (when define-remove-p (intern-conc *package* base-name "-REMOVE")))
         (representative-key (if define-representative-p
                                 ;; We need this function whether they want it or not
                                 (intern-conc *package* node-base-type "-REPRESENTATIVE")
                                 (gensym "NODE-REPRESENTATIVE")))
         (checker (intern-conc *package* "CHECK-" base-name))
         (with-key (intern-conc *package* node-base-type "-WITH-KEY"))
         (without-key (intern-conc *package* node-base-type "-WITHOUT-KEY"))
         (with-equal (gensym "NODE-WITH-EQUAL"))
         (without-equal (gensym "NODE-WITHOUT-EQUAL"))
         (with-unequal (gensym "NODE-WITH-UNEQUAL"))
         (without-unequal (gensym "NODE-WITHOUT-UNEQUAL"))
         (remove-min (gensym "REMOVE-MIN"))
         (append-children (gensym "APPEND-CHILD-TREES"))
         (balance-needed-p (gensym "BALANCE-NEEDED-P"))
         (values (gensym "VALUES"))
         (result (gensym "RESULT"))
         (comparator (gensym "COMPARATOR"))
         (comparison (gensym "COMPARISON"))
         (tree (gensym "TREE"))
         (stream (gensym "STREAM"))
         (other (gensym "OTHER"))
         (check (gensym "CHECK"))
         (sublist (gensym "SUBLIST"))
         (key (gensym "KEY"))
         (value (gensym "VALUE"))
         (value-list (when map-p `(,value)))
         (min (gensym "MIN"))
         (without-min (gensym "WITHOUT-MIN"))
         (body (gensym "BODY"))
         (fn (gensym "FN"))
         (builder (gensym "BUILDER"))
         (alist (make-symbol "ALIST"))
         (plist (make-symbol "PLIST"))
         (items (make-symbol "ITEMS")))

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

         ((,node-base-type (:copier nil))
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

       (defun ,with-unequal (,comparator ,tree ,key ,@value-list)
         (etypecase ,tree
           (,node-n-type
            (,copy-n-type ,tree
                          :values (,(if map-p 'list-map-with 'list-set-with)
                                   ,comparator
                                   (,n-type-values ,tree)
                                   ,key
                                   ,@value-list)))
           (,node-1-type
            (let ((,result (structure-convert (,node-n-type ,node-1-type)
                                              ,tree
                                              :values ,(if map-p
                                                           `(list-map
                                                             ,comparator
                                                             (,1-type-key ,tree)
                                                             (,1-type-value ,tree)
                                                             ,key
                                                             ,value)
                                                           `(list-set
                                                             ,comparator
                                                             (,1-type-key ,tree)
                                                             ,key)))))
              (assert (cdr (,n-type-values ,result)))
              ,result))))

       (defun ,without-unequal (,comparator ,tree ,key)
         (etypecase ,tree
           (,node-n-type
            (let ((,values (,(if map-p 'list-map-without 'list-set-without)
                            ,comparator
                            (,n-type-values ,tree)
                            ,key)))
              (if (cdr ,values)
                  (,copy-n-type ,tree :values ,values)
                  (structure-convert (,node-1-type ,node-n-type)
                                     ,tree
                                     ,@(if map-p
                                           `(:key (car (car ,values)) :value (cdr (car ,values)))
                                           `(:key (car ,values)))))))
           (,node-1-type
            ,tree)))

       (declaim (inline ,with-equal))
       (defun ,with-equal (,comparator ,tree ,key ,@value-list)
         ,@(if map-p
               `((etypecase ,tree
                   (,node-n-type
                    (,with-unequal ,comparator ,tree ,key ,@value-list))
                   (,node-1-type
                    (,copy-1-type ,tree :value ,value))))
               `((declare (ignore ,comparator ,key ,@value-list))
                 ,tree)))

       (declaim (inline ,without-equal))
       (defun ,without-equal (,comparator ,tree ,key)
         (etypecase ,tree
           (,node-n-type
            (,without-unequal ,comparator ,tree ,key))
           (,node-1-type
            (,nil-type))))

       (declaim (inline ,with-key))
       (defun ,with-key (,comparator ,tree ,comparison ,key ,@value-list)
         (ecase ,comparison
           (:equal
            (,with-equal ,comparator ,tree ,key ,@value-list))
           (:unequal
            (,with-unequal ,comparator ,tree ,key ,@value-list))))

       (declaim (inline ,without-key))
       (defun ,without-key (,comparator ,tree ,comparison ,key)
         (ecase ,comparison
           (:equal
            (,without-equal ,comparator ,tree ,key))
           (:unequal
            (,without-unequal ,comparator ,tree ,key))))

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

       ,@(when define-insert-p
           `((defun ,insert (,comparator ,tree ,key ,@value-list)
               (etypecase ,tree
                 (,nil-type
                  (values (,make-1-type :key ,key ,@(when map-p `(:value ,value)))
                          t))
                 ((or ,node-1-type ,node-n-type)
                  (ecase (funcall ,comparator ,key (,representative-key ,tree))
                    (:less
                     (multiple-value-bind (,result ,balance-needed-p) (,insert ,comparator (,node-left ,tree) ,key ,@value-list)
                       (if ,balance-needed-p
                           (values (,insert-left-balancer ,tree :left ,result) t)
                           (values (,node-copy ,tree :left ,result) nil))))
                    (:greater
                     (multiple-value-bind (,result ,balance-needed-p) (,insert ,comparator (,node-right ,tree) ,key ,@value-list)
                       (if ,balance-needed-p
                           (values (,insert-right-balancer ,tree :right ,result) t)
                           (values (,node-copy ,tree :right ,result) nil))))
                    (:equal
                     (values (,with-equal ,comparator ,tree ,key ,@value-list) nil))
                    (:unequal
                     (values (,with-unequal ,comparator ,tree ,key ,@value-list) nil))))))))

       ,@(when (and define-insert-p define-maker-p)
           (if map-p
               `((defun ,maker (,comparator &key ,alist ,plist)
                   (let ((,tree (,nil-type)))
                     (loop :while ,plist
                           :for ,key = (pop ,plist)
                           :for ,value = (if ,plist (pop ,plist) (error "Odd number of items in plist"))
                           :do (setf ,tree (,insert ,comparator ,tree ,key ,value)))
                     (dolist (,value ,alist)
                       (setf ,tree (,insert ,comparator ,tree (car ,value) (cdr ,value))))
                     ,tree)))
               `((defun ,maker (,comparator &key ,items)
                   (let ((,tree (,nil-type)))
                     (dolist (,value ,items)
                       (setf ,tree (,insert ,comparator ,tree ,value)))
                     ,tree)))))

       ,@(when define-lookup-p
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
           `((defun ,remove-min (,tree)
               (etypecase ,tree
                 (,nil-type
                  (error "nil tree has no min"))
                 ((or ,node-1-type ,node-n-type)
                  (if (,nil-type-p (,node-left ,tree))
                      (values ,tree (,node-right ,tree))
                      (multiple-value-bind (,result ,value) (,remove-min (,node-left ,tree))
                        (values ,result (,remove-left-balancer ,tree :left ,value)))))))

             (defun ,append-children (,tree)
               (multiple-value-bind (,min ,without-min) (,remove-min (,node-right ,tree))
                 ;; Now we make a version of tree that has all the
                 ;; same metadata but the value(s) of min.  We're
                 ;; just migrating the min values up to the position
                 ;; of tree.  Afterwards, we'll run the balancer to
                 ;; update the metadata.
                 (setf ,tree (etypecase ,tree
                               (,node-1-type
                                (etypecase ,min
                                  (,node-1-type
                                   (,node-copy ,tree :key (,1-type-key ,min) ,@(when map-p `(:value (,1-type-value ,min)))))
                                  (,node-n-type
                                   (structure-convert (,node-n-type ,node-1-type) ,tree :values (,n-type-values ,min)))))
                               (,node-n-type
                                (etypecase ,min
                                  (,node-1-type
                                   (structure-convert (,node-1-type ,node-n-type) ,tree :key (,1-type-key ,min)
                                                      ,@(when map-p `(:value (,1-type-value ,min)))))
                                  (,node-n-type
                                   (,node-copy ,tree :values (,n-type-values ,min)))))))
                 (,remove-right-balancer ,tree :right ,without-min)))

             (defun ,remove (,comparator ,tree ,key)
               (etypecase ,tree
                 (,nil-type
                  (values ,tree nil))
                 ((or ,node-1-type ,node-n-type)
                  (ecase (funcall ,comparator ,key (,representative-key ,tree))
                    (:less
                     (multiple-value-bind (,result ,balance-needed-p) (,remove ,comparator (,node-left ,tree) ,key)
                       (if ,balance-needed-p
                           (values (,remove-left-balancer ,tree :left ,result) t)
                           (values (,node-copy ,tree :left ,result)))))
                    (:greater
                     (multiple-value-bind (,result ,balance-needed-p) (,remove ,comparator (,node-right ,tree) ,key)
                       (if ,balance-needed-p
                           (values (,remove-right-balancer ,tree :right ,result) t)
                           (values (,node-copy ,tree :right ,result)))))
                    (:equal
                     (let ((,result (,without-equal ,comparator ,tree ,key)))
                       (if (,nil-type-p ,result)
                           (values (cond
                                     ((,nil-type-p (,node-left ,tree))
                                      (,node-right ,tree))
                                     ((,nil-type-p (,node-right ,tree))
                                      (,node-left ,tree))
                                     (t
                                      (,append-children ,tree)))
                                   t)
                           (values ,result nil))))
                    (:unequal
                     (values (,without-unequal ,comparator ,tree ,key) nil))))))))

       (defun ,do-tree-f (,tree ,fn)
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
         (,do-tree-f (,node-right ,tree) ,fn))

       (defmacro ,do-tree ((,key ,@value-list ,tree &optional ,result) &body ,body)
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

       (defmethod print-node-properties ((,tree ,base-name) ,stream)
         (format ,stream "label=\"~A\" shape=box"
                 (unless (,nil-type-p ,tree)
                   (etypecase ,tree
                     (,node-1-type
                      ,(if map-p
                           `(cons (,1-type-key ,tree) (,1-type-value ,tree))
                           `(,1-type-key ,tree)))
                     (,node-n-type
                      (,n-type-values ,tree))))))

       (defmethod print-graphviz ((,tree ,base-name) ,stream)
         (let ((,value (incf *graph-id*)))
           (format ,stream "ID~A [" ,value)
           (print-node-properties ,tree ,stream)
           (format ,stream "]~%")
           (unless (,nil-type-p ,tree)
             (let ((,result (print-graphviz (,node-left ,tree) ,stream)))
               (format ,stream "ID~A -> ID~A~%" ,value ,result))
             (let ((,result (print-graphviz (,node-right ,tree) ,stream)))
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

       (defmethod ,checker (,tree ,comparator)
         (unless (,nil-type-p ,tree)
           (labels
               ((,check (,key)
                  (,do-tree (,other ,@value-list (,node-left ,tree))
                    (declare (ignore ,@value-list))
                    (cassert (eq :less (funcall ,comparator ,other ,key))
                             nil "All left children must be less than all their parents"))
                  (,do-tree (,other ,@value-list (,node-right ,tree))
                    (declare (ignore ,@value-list))
                    (cassert (eq :greater (funcall ,comparator ,other ,key))
                             nil "All right children must be greater than all their parents"))))
             (declare (dynamic-extent #',check))

             (etypecase ,tree
               (,node-1-type
                (,check (,1-type-key ,tree)))
               (,node-n-type
                (cassert (cdr (,n-type-values ,tree))
                         nil "n-type nodes must have multiple values")
                (dolist (,value (,n-type-values ,tree))
                  (,check ,(if map-p `(car ,value) value)))
                (loop :for ,sublist :on (,n-type-values ,tree)
                      :for ,key = ,(if map-p `(car (car ,sublist)) `(car ,sublist)) :do
                        (loop :for ,other :in (cdr ,sublist) :do
                          (cassert (eq :unequal (funcall ,comparator ,key ,(if map-p `(car ,other) other)))
                                   nil "Peer values in n-type must be mutually :unequal")))))
             (,checker (,node-left ,tree) ,comparator)
             (,checker (,node-right ,tree) ,comparator))))

       ',base-name)))
