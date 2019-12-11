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

(defpackage :pfds.shcl.io/immutable-structure
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/compare #:define-type-id)
  (:import-from :pfds.shcl.io/structure-mop
   #:find-struct-class
   #:struct-class
   #:struct-class-copier
   #:struct-class-name
   #:struct-class-effective-slots
   #:struct-class-effective-slots-table
   #:struct-class-constructors
   #:effective-slot-readers
   #:effective-slot-name
   #:struct-definition-options
   #:struct-definition-form
   #:direct-slot-definition-class
   #:direct-slot
   #:define-struct)
  (:export
   #:immutable-structure
   #:define-immutable-structure
   #:define-adt
   #:structure-convert))
(in-package :pfds.shcl.io/immutable-structure)

(defclass immutable-structure (struct-class)
  ())

(defclass immutable-slot (direct-slot)
  ()
  (:default-initargs :read-only t))

(defmethod shared-initialize ((slot direct-slot) slots &key (read-only t read-only-p))
  (when read-only-p
    (unless read-only
      (error "Immutable slots must be read only")))
  (call-next-method))

(defmethod direct-slot-definition-class ((struct immutable-structure))
  (find-class 'immutable-slot))

(defmethod struct-definition-options ((struct immutable-structure))
  (let ((options (call-next-method)))
    (cons '(:copier nil)
          (remove-if (lambda (item)
                       (or (eq item :copier)
                           (and (consp item)
                                (eq (car item) :copier))))
                     options))))

(defmethod struct-definition-form ((struct immutable-structure))
  (let ((copier (struct-class-copier struct)))
    (if copier
        `(progn
           ,(call-next-method)
           (define-changer-function ,copier ,(struct-class-name struct))
           ',(struct-class-name struct))
        (call-next-method))))

(defmacro define-immutable-structure (name-and-options &body slots)
  (when (symbolp name-and-options)
    (setf name-and-options (list name-and-options)))
  `(define-struct (,(car name-and-options)
                   (:metaclass immutable-structure)
                   ,@(cdr name-and-options))
     ,@slots))

(defmacro define-adt (name-and-options common-slots &body subtypes)
  (unless subtypes
    (warn "An ADT without subtypes isn't very useful"))
  (when (symbolp name-and-options)
    (setf name-and-options (list name-and-options)))
  (destructuring-bind (base-name &rest options) name-and-options
    (labels
        ((handle-subtype (definition)
           (destructuring-bind (name &rest slots) definition
             (when (symbolp name)
               (setf name `(,name)))
             (destructuring-bind (real-name &rest options) name
               (setf name `(,real-name (:include ,base-name) (:metaclass immutable-structure)
                                       ,@options)))
             `(define-struct ,name ,@slots))))
      `(progn
         (define-struct (,base-name (:metaclass immutable-structure)
                                       (:constructor nil)
                                       (:copier nil)
                                       ,@options)
           ,@common-slots)
         ,@(mapcar #'handle-subtype subtypes)))))

(defgeneric clone (object &key))

(defmacro define-changer-function (name structure-name)
  (let* ((metaobject (find-struct-class structure-name :error-p t))
         (constructor (or (loop :for potential :in (struct-class-constructors metaobject)
                                :do (when (symbolp potential)
                                      (return potential)))
                          (error "couldn't find an eligible constructor")))
         (slots (struct-class-effective-slots metaobject))
         (slot-names (mapcar (lambda (s) (symbol-name (effective-slot-name s))) slots))
         (slot-readers (mapcar (lambda (s) (car (effective-slot-readers s))) slots))
         (slot-vars (mapcar 'gensym slot-names))
         (slot-keywords (mapcar (lambda (name) (intern name :keyword)) slot-names))
         (object (make-symbol "OBJECT")) ;; so it doesn't get an ugly number
         (key-args (mapcar (lambda (keyword var reader)
                             `((,keyword ,var) (,reader ,object)))
                           slot-keywords slot-vars slot-readers)))

    (let ((body `(if (and ,@(mapcar (lambda (var reader)
                                      `(eql ,var (,reader ,object)))
                                    slot-vars slot-readers))
                     ,object
                     (,constructor
                      ,@(mapcan (lambda (keyword var)
                                  `(,keyword ,var))
                                slot-keywords slot-vars)))))
      `(progn
         (defmethod clone ((,object ,structure-name) &key ,@key-args)
           (unless (eq (class-of ,object) ,(find-class structure-name))
             (warn "No clone method for ~A, falling back to ~A cloner" (class-of ,object) ',structure-name))
           ,body)
         (defun ,name (,object &key ,@key-args)
           (if (eq (class-of ,object) ,(find-class structure-name))
               ,body
               (clone ,object ,@(mapcan (lambda (keyword var)
                                          `(,keyword ,var))
                                        slot-keywords slot-vars))))
         ',name))))

(defmacro structure-convert ((to-name from-name) object &rest initargs)
  (let* ((from (find-struct-class from-name :error-p t))
         (from-slots-table (struct-class-effective-slots-table from))
         (to (find-struct-class to-name :error-p t))
         (to-slots-table (struct-class-effective-slots-table to))
         (to-constructor (or (loop :for potential :in (struct-class-constructors to)
                                   :do (when (symbolp potential)
                                         (return potential)))
                             (error "couldn't find an eligible constructor")))
         (object-sym (gensym "OBJECT")))
    `(let ((,object-sym ,object))
       (,to-constructor
        ,@initargs
        ,@(loop :for name :being :the :hash-keys :of to-slots-table
                  :using (hash-value slot)
                :for from-slot = (gethash name from-slots-table)
                :when from-slot
                  :nconc
                `(,(intern name :keyword)
                  (,(car (effective-slot-readers from-slot)) ,object-sym)))))))
