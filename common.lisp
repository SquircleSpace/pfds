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
  (:import-from :pfds.shcl.io/immutable-structure
   #:immutable-structure #:define-adt #:structure-convert
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/structure-mop
   #:define-struct)
  (:export
   #:is-empty #:empty #:with-member
   #:define-interface #:compare #:compare* #:define-type-id
   #:immutable-structure #:to-list #:compare-objects
   #:structure-convert #:define-adt #:define-struct))
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
