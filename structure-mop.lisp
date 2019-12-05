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

(defpackage :pfds.shcl.io/structure-mop
  (:use :common-lisp)
  (:export
   #:find-structure-metaobject
   #:structure-metaobject
   #:structure-metaobject-name
   #:structure-metaobject-superclass
   #:structure-metaobject-conc-name
   #:structure-metaobject-predicate
   #:structure-metaobject-constructors
   #:structure-metaobject-copier
   #:structure-metaobject-direct-slots
   #:structure-metaobject-direct-slots-table
   #:structure-metaobject-effective-slots
   #:structure-metaobject-effective-slots-table
   #:direct-slot
   #:direct-slot-name
   #:direct-slot-type
   #:direct-slot-read-only
   #:effective-slot
   #:effective-slot-name
   #:effective-slot-type
   #:effective-slot-read-only
   #:effective-slot-readers))
(in-package :pfds.shcl.io/structure-mop)

(defclass structure-metaobject ()
  ((name
    :initarg :name
    :reader structure-metaobject-name)
   (superclass
    :initarg :superclass
    :reader structure-metaobject-superclass)
   (conc-name
    :initarg :conc-name
    :reader structure-metaobject-conc-name)
   (predicate
    :initarg :predicate
    :reader structure-metaobject-predicate)
   (constructors
    :initarg :constructors
    :reader structure-metaobject-constructors)
   (copier
    :initarg :copier
    :reader structure-metaobject-copier)
   (direct-slots-table
    :initarg :direct-slots-table
    :reader structure-metaobject-direct-slots-table)
   (effective-slots-table
    :initarg :effective-slots-table
    :reader structure-metaobject-effective-slots-table)))

(defun structure-metaobject-direct-slots (object)
  (loop :for value :being :the :hash-values
          :of (structure-metaobject-direct-slots-table object)
        :collect value))

(defun structure-metaobject-effective-slots (object)
  (loop :for value :being :the :hash-values
          :of (structure-metaobject-effective-slots-table object)
        :collect value))

(defclass direct-slot ()
  ((name
    :initarg :name
    :reader direct-slot-name)
   (type
    :initarg :type
    :reader direct-slot-type)
   (read-only
    :initarg :read-only
    :reader direct-slot-read-only)))

(defun intern-conc (package &rest things)
  (let ((name (with-output-to-string (stream)
                (dolist (thing things)
                  (etypecase thing
                    (string
                     (write-string thing stream))
                    (symbol
                     (write-string (symbol-name thing) stream)))))))
    (if package
        (intern name package)
        (make-symbol name))))

(defun intern-direct-slot-reader (package conc-name name-string)
  (assert package)
  (if conc-name
      (intern-conc package conc-name name-string)
      (intern name-string package)))

(defun make-direct-slot (direct-slot-list)
  (when (symbolp direct-slot-list)
    (setf direct-slot-list (list direct-slot-list)))

  (locally
      #+sbcl (declare (sb-ext:muffle-conditions style-warning))
    (destructuring-bind (name &optional initform &key read-only (type t))
        direct-slot-list
      (declare (ignore initform))
      (setf name (symbol-name name))
      (make-instance 'direct-slot
                     :name name
                     :type type
                     :read-only read-only))))

(defclass effective-slot ()
  ((name
    :initarg :name
    :reader effective-slot-name)
   (type
    :initarg :type
    :reader effective-slot-type)
   (read-only
    :initarg :read-only
    :reader effective-slot-read-only)
   (readers
    :initarg :readers
    :reader effective-slot-readers)))

(defun structure-metaobject-readers (structure-metaobject)
  (let ((result (make-hash-table :test 'eq)))
    (loop :for slot :being :the :hash-values
            :of (structure-metaobject-effective-slots-table structure-metaobject)
          :do (dolist (reader (effective-slot-readers slot))
                (setf (gethash reader result) slot)))
    result))

(defun resolve-effective-slot (slot-name superclass &key direct-slot direct-slot-reader)
  (let* ((superclass-slots
           (if superclass
               (structure-metaobject-effective-slots-table superclass)
               (make-hash-table :test 'equal)))
         (superclass-readers
           (if superclass
               (structure-metaobject-readers superclass)
               (make-hash-table :test 'eq)))
         (superclass-slot
           (gethash slot-name superclass-slots))
         (superclass-slot-readers
           (when superclass-slot
             (effective-slot-readers superclass-slot)))
         (readers
           (if (gethash direct-slot-reader superclass-readers)
               superclass-slot-readers
               (cons direct-slot-reader superclass-slot-readers)))
         (type (cond
                 ;; Okay, this isn't actually right.  We should be
                 ;; validating that its a proper subtype... but.. I
                 ;; mean... isn't that the job of the code evaluating
                 ;; the defstruct form?  We can be a bit sloppy.
                 ((and superclass-slot (or (not direct-slot)
                                           (eq t (direct-slot-type direct-slot))))
                  (effective-slot-type superclass-slot))
                 (direct-slot
                  (direct-slot-type direct-slot))
                 (t
                  t)))
         (read-only (or (and direct-slot (direct-slot-read-only direct-slot))
                        (and superclass-slot (effective-slot-read-only superclass-slot)))))

    (make-instance
     'effective-slot
     :name slot-name
     :type type
     :read-only read-only
     :readers readers)))

(defvar *structure-metaobjects* (make-hash-table :test 'eq))

(defun find-structure-metaobject (name)
  (nth-value 0 (gethash name *structure-metaobjects*)))

(defun (setf find-structure-metaobject) (value name)
  (setf (gethash name *structure-metaobjects*) value))

(defun make-structure-metaobject (name-and-options slots)
  (when (symbolp name-and-options)
    (setf name-and-options (list name-and-options)))

  (destructuring-bind (name &rest options) name-and-options
    (let* ((unspecified (gensym "UNSPECIFIED"))
           include
           (include-direct-slots (make-hash-table :test 'equal))
           (conc-name unspecified)
           (predicate unspecified)
           (copier unspecified)
           constructors
           (default-constructor t)
           (direct-slots (make-hash-table :test 'equal))
           (effective-slots (make-hash-table :test 'equal))
           superclass)
      (dolist (option options)
        (when (symbolp option)
          (setf option (list option)))
        (destructuring-bind (option-name &rest option-args) option
          (ecase option-name
            (:copier
             (when option-args
               (setf copier (car option-args))))
            (:conc-name
             (setf conc-name (car option-args)))
            (:constructor
                (setf default-constructor nil)
                (cond
                  ((null option-args)
                   (push (intern-conc *package* "MAKE-" name) constructors))
                  ((null (car option-args)))
                  ((null (cdr option-args))
                   (push (car option-args) constructors))
                  (t
                   (push option-args constructors))))
            (:predicate
             (when option-args
               (setf predicate (car option-args))))
            (:include
             (setf include (car option-args))
             (dolist (slot-description (cdr option-args))
               (let ((slot (make-direct-slot slot-description)))
                 (setf (gethash (direct-slot-name slot) include-direct-slots) slot)))))))

      (when (eq unspecified conc-name)
        (setf conc-name (intern-conc nil name "-")))

      (when (eq unspecified predicate)
        (setf predicate (intern-conc *package* name "-P")))

      (when (eq unspecified copier)
        (setf copier (intern-conc *package* "COPY-" name)))

      (when default-constructor
        (push (intern-conc *package* "MAKE-" name) constructors))

      (when include
        (setf superclass (find-structure-metaobject include))
        (assert superclass))

      (dolist (slot-description slots)
        (let ((slot (make-direct-slot slot-description)))
          (setf (gethash (direct-slot-name slot) direct-slots) slot)))

      (labels
          ((store (slot)
             (assert (not (gethash (effective-slot-name slot) effective-slots)))
             (setf (gethash (effective-slot-name slot) effective-slots) slot))
           (reader-for (name)
             (intern-direct-slot-reader *package* conc-name name)))

        (when superclass
          (loop :for direct-slot :being :the :hash-values :of include-direct-slots
                :do (store (resolve-effective-slot (direct-slot-name direct-slot)
                                                   superclass
                                                   :direct-slot direct-slot
                                                   :direct-slot-reader (reader-for (direct-slot-name direct-slot)))))

          (loop :for effective-slot :being :the :hash-values
                  :of (structure-metaobject-effective-slots-table superclass)
                :do (let ((name (effective-slot-name effective-slot)))
                      (unless (gethash name include-direct-slots)
                        (let* ((reader (reader-for (effective-slot-name effective-slot)))
                               (slot (resolve-effective-slot name superclass
                                                             :direct-slot-reader reader)))
                          (store slot))))))

        (loop :for direct-slot :being :the :hash-values
                :of direct-slots
              :do (let ((name (direct-slot-name direct-slot)))
                    (store (resolve-effective-slot name superclass
                                                   :direct-slot direct-slot
                                                   :direct-slot-reader (reader-for name))))))

      (make-instance 'structure-metaobject
                     :effective-slots-table effective-slots
                     :direct-slots-table direct-slots
                     :copier copier
                     :constructors constructors
                     :predicate predicate
                     :conc-name conc-name
                     :superclass superclass
                     :name name))))

(defmacro register-structure (name-and-options &body slots)
  (let ((metaobject (gensym "METAOBJECT")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,metaobject (make-structure-metaobject ',name-and-options ',slots)))
         (setf (find-structure-metaobject (structure-metaobject-name ,metaobject))
               ,metaobject)
         (structure-metaobject-name ,metaobject)))))
