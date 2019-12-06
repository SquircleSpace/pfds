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

(defpackage :pfds.shcl.io/common
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/compare
   #:compare #:define-type-id #:compare* #:compare-objects)
  (:import-from :pfds.shcl.io/structure-mop
   #:register-structure #:find-structure-metaobject
   #:structure-metaobject-constructors #:structure-metaobject-effective-slots
   #:effective-slot-name #:effective-slot-readers)
  (:export
   #:is-empty #:empty #:with-member
   #:define-interface #:compare #:compare* #:define-type-id
   #:define-structure #:to-list #:compare-objects))
(in-package :pfds.shcl.io/common)

(defgeneric to-list (object))

(defgeneric is-empty (container))
(defgeneric empty (container))
(defgeneric with-member (container item))

(defmacro define-interface (name &body functions)
  `(progn
     ,@(loop
         :for thing :in functions
         :collect
         (etypecase thing
           (symbol `',thing)
           (cons
            (if (eq (car thing) 'defgeneric)
                thing
                (error "Invalid interface")))))
     ',name))

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
               (setf name `(,real-name (:include ,base-name) ,@options)))
             `(define-structure ,name ,@slots))))
      `(progn
         (define-structure (,base-name (:constructor nil) ,@options)
           ,@common-slots)
         ,@(mapcar #'handle-subtype subtypes)))))

(defmacro define-structure (name-and-options &body slots)
  (when (symbolp name-and-options)
    (setf name-and-options (list name-and-options)))
  (destructuring-bind (name &rest options) name-and-options
    (labels
        ((handle-slot (slot-definition)
           (etypecase slot-definition
             (symbol
              `(,slot-definition nil :read-only t))
             (cons
              (destructuring-bind (name &optional default-value &rest options) slot-definition
                `(,name ,default-value :read-only t ,@options)))))
         (default-copier-name ()
           (intern (format nil "COPY-~A" (symbol-name name))))
         (copier-option-p (option)
           (or (eq option :copier)
               (and (consp option)
                    (eq (car option) :copier)))))
      (let (copier
            copier-set)
        (dolist (option options)
          (when (copier-option-p option)
            (when copier-set
              (error ":copier may only be specified once"))
            (setf copier-set t)
            (setf copier (if (or (eq option :copier)
                                 (null (cdr option)))
                             (default-copier-name)
                             (second option)))))

        (unless copier-set
          (setf copier (default-copier-name)))

        (setf options (cons '(:copier nil) (remove-if #'copier-option-p options)))
        `(progn
           (defstruct (,name ,@options)
             ,@(mapcar #'handle-slot slots))
           (register-structure (,name ,@options) ,@slots)
           ,@(when copier
               `((declaim (inline ,copier))
                 (define-changer-function ,copier ,name)))
           (define-type-id ,name)
           ',name)))))

(defmacro define-changer-function (name structure-name)
  (let* ((metaobject (or (find-structure-metaobject structure-name)
                         (error "couldn't lookup structure")))
         (constructor (or (loop :for potential :in (structure-metaobject-constructors metaobject)
                                :do (when (symbolp potential)
                                      (return potential)))
                          (error "couldn't find an eligible constructor")))
         (slots (structure-metaobject-effective-slots metaobject))
         (slot-names (mapcar 'effective-slot-name slots))
         (slot-readers (mapcar (lambda (s) (car (effective-slot-readers s))) slots))
         (slot-vars (mapcar 'gensym slot-names))
         (slot-keywords (mapcar (lambda (name) (intern name :keyword)) slot-names))
         (object (make-symbol "OBJECT")) ;; so it doesn't get an ugly number
         (key-args (mapcar (lambda (keyword var reader)
                             `((,keyword ,var) (,reader ,object)))
                           slot-keywords slot-vars slot-readers)))
    (unless slots
      (return-from define-changer-function
        `(defun ,name (,object)
           ,object)))

    `(defun ,name (,object &key ,@key-args)
       (if (and ,@(mapcar (lambda (var reader)
                            `(eql ,var (,reader ,object)))
                          slot-vars slot-readers))
           ,object
           (,constructor
            ,@(mapcan (lambda (keyword var)
                        `(,keyword ,var))
                      slot-keywords slot-vars))))))
