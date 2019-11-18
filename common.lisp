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
   #:compare #:define-type-id #:compare*)
  (:export
   #:is-empty #:empty #:with-member #:without-member #:is-member
   #:define-interface #:compare #:compare* #:define-type-id))
(in-package :pfds.shcl.io/common)

(defgeneric is-empty (container))
(defgeneric empty (container))
(defgeneric with-member (container item))
(defgeneric without-member (container item))
(defgeneric is-member (container item))

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

(defmacro define-adt (base-name common-slots &body subtypes)
  (labels
      ((handle-subtype (definition)
         (destructuring-bind (name &rest slots) definition
           (when (symbolp name)
             (setf name `(,name)))
           (destructuring-bind (real-name &rest options) name
             (setf name `(,real-name (:include ,base-name) ,@options))
             `(progn
                (defstruct ,name ,@(mapcar #'handle-slot slots))
                (define-type-id ,real-name)))))
       (handle-slot (slot-definition)
         (etypecase slot-definition
           (symbol
            `(,slot-definition nil :read-only t))
           (cons
            (destructuring-bind (name default-value &rest options) slot-definition
              `(,name ,default-value :read-only t ,@options))))))
    `(progn
       (progn
         (defstruct (,base-name (:constructor ,(gensym (symbol-name base-name))))
           ,@(mapcar #'handle-slot common-slots))
         (define-type-id ,base-name))
       ,@(mapcar #'handle-subtype subtypes))))
