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
   #:find-struct-class
   #:struct-class
   #:struct-class-name
   #:struct-class-superclass
   #:struct-class-slot-overrides
   #:struct-class-slot-overrides-table
   #:struct-class-conc-name
   #:struct-class-predicate
   #:struct-class-constructors
   #:struct-class-copier
   #:struct-class-package
   #:struct-class-direct-slots
   #:struct-class-direct-slots-table
   #:struct-class-effective-slots
   #:struct-class-effective-slots-table
   #:direct-slot
   #:direct-slot-name
   #:direct-slot-type
   #:direct-slot-read-only
   #:direct-slot-initform
   #:direct-slot-reader
   #:effective-slot
   #:effective-slot-name
   #:effective-slot-type
   #:effective-slot-read-only
   #:effective-slot-readers
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:compute-effective-slot-initargs
   #:compute-effective-slots
   #:make-struct-metaobject
   #:slot-definition-form
   #:struct-definition-form
   #:struct-definition-options
   #:define-struct))
(in-package :pfds.shcl.io/structure-mop)

(defclass struct-class ()
  ((name
    :initarg :name
    :reader struct-class-name
    :initform (error "name is required"))
   (superclass
    :initarg :superclass
    :reader struct-class-superclass
    :initform nil)
   (slot-overrides
    :initarg :slot-overrides
    :reader struct-class-slot-overrides
    :initform nil)
   (slot-overrides-table
    :reader struct-class-slot-overrides-table)
   (conc-name
    :initarg :conc-name
    :reader struct-class-conc-name
    :initform nil)
   (predicate
    :initarg :predicate
    :reader struct-class-predicate
    :initform nil)
   (constructors
    :initarg :constructors
    :reader struct-class-constructors
    :initform nil)
   (copier
    :initarg :copier
    :reader struct-class-copier
    :initform nil)
   (package
    :initarg :package
    :reader struct-class-package
    :initform *package*)
   (direct-slots
    :initarg :direct-slots
    :initform nil
    :reader struct-class-direct-slots)
   (direct-slots-table
    :reader struct-class-direct-slots-table)
   (effective-slots
    :reader struct-class-effective-slots)
   (effective-slots-table
    :reader struct-class-effective-slots-table)))

(defclass direct-slot ()
  ((name
    :initarg :name
    :reader direct-slot-name
    :initform (error "name is required"))
   (type
    :initarg :type
    :reader direct-slot-type
    :initform :unspecific)
   (read-only
    :initarg :read-only
    :reader direct-slot-read-only
    :initform :unspecific)

   ;; In standard Common Lisp, struct slots can be uninitialized.  The
   ;; value stored in an uninitialized slot is undefined.  I guess the
   ;; idea is to avoid wasting cycles storing a default value if you
   ;; intend to write in a different value anyway.  Allowing for that
   ;; flexibility is awkward -- especially since the DEFSTRUCT macro
   ;; doesn't have a way to specify other options (e.g. type) without
   ;; also specifying the initform.  What if a DIRECT-SLOT has a type
   ;; but no initform?!  Since the value of an uninitialized slot is
   ;; undefined, we are free to provide our own semantics and default
   ;; it to nil.

   ;; Just as a fun fact... SBCL, CCL, and ECL seem to do the same
   ;; thing and assume a nil value... except in the case of overrides
   ;; for a superclass's slots.  In SBCL, overriding a slot but
   ;; providing no initform results in no change to that slot's value.
   ;; In ECL and CCL it results in a nil value.
   (initform
    :initarg :initform
    :reader direct-slot-initform
    :initform nil)
   (reader
    :initarg :reader
    :reader direct-slot-reader
    :initform (error "required"))))

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

(defgeneric direct-slot-definition-class (struct-metaobject))

(defmethod direct-slot-definition-class ((struct-class struct-class))
  (find-class 'direct-slot))

(defgeneric effective-slot-definition-class (struct-metaobject))

(defmethod effective-slot-definition-class ((struct-class struct-class))
  (find-class 'effective-slot))

(defun make-slot-table (name-getter slots)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (slot slots)
      (let ((name (funcall name-getter slot)))
        (check-type name symbol)
        (setf name (symbol-name name))
        (when (nth-value 1 (gethash name table))
          (error "Duplicate slot name encountered: ~A" name))
        (setf (gethash name table) slot)))
    table))

(defgeneric compute-effective-slot-initargs (struct-class direct-slot superclass-effective-slot))

(defmethod compute-effective-slot-initargs ((struct-class struct-class) (direct-slot null) (superclass-effective-slot effective-slot))
  (let ((reader (intern-conc (struct-class-package struct-class)
                             (struct-class-conc-name struct-class)
                             (effective-slot-name superclass-effective-slot))))
    (list :name (effective-slot-name superclass-effective-slot)
          :type (effective-slot-type superclass-effective-slot)
          :read-only (effective-slot-read-only superclass-effective-slot)
          :readers (cons reader (effective-slot-readers superclass-effective-slot)))))

(defmethod compute-effective-slot-initargs ((struct-class struct-class) (direct-slot direct-slot) (superclass-effective-slot null))
  (let ((type (direct-slot-type direct-slot))
        (read-only (direct-slot-read-only direct-slot)))
    (when (eq :unspecific type)
      (setf type t))
    (when (eq :unspecific read-only)
      (setf read-only nil))

    (list :name (direct-slot-name direct-slot)
          :type type
          :read-only read-only
          :readers (list (direct-slot-reader direct-slot)))))

(defmethod compute-effective-slot-initargs ((struct-class struct-class) (direct-slot direct-slot) (superclass-effective-slot effective-slot))
  (let ((type (direct-slot-type direct-slot))
        (read-only (direct-slot-read-only direct-slot)))
    (when (eq :unspecific type)
      (setf type (effective-slot-type superclass-effective-slot)))
    (cond
      ((eq :unspecific read-only)
       (setf read-only (effective-slot-read-only superclass-effective-slot)))
      ((and (effective-slot-read-only superclass-effective-slot)
            (not read-only))
       (error "Cannot override read-only in a subclass")))
    (list :name (direct-slot-name direct-slot)
          :type type
          :read-only read-only
          :readers (cons (direct-slot-reader direct-slot) (effective-slot-readers superclass-effective-slot)))))

(defgeneric compute-effective-slots (struct-class))

(defmethod compute-effective-slots ((struct-class struct-class))
  (let* (effective-slots
         (superclass (struct-class-superclass struct-class))
         (superclass-slots (when superclass
                             (struct-class-effective-slots-table superclass)))
         (overrides-table (struct-class-slot-overrides-table struct-class))
         (direct-slots (struct-class-direct-slots struct-class))
         (effective-class (effective-slot-definition-class struct-class)))

    (when (and (plusp (hash-table-count overrides-table)) (not superclass))
      (error "Cannot have slot overrides without a superclass"))
    (when superclass
      (check-type superclass-slots hash-table))
    (labels
        ((lookup-super (slot)
           (when superclass-slots
             (gethash (symbol-name (direct-slot-name slot)) superclass-slots)))
         (make-effective (direct effective)
           (apply 'make-instance effective-class
                  (compute-effective-slot-initargs struct-class direct effective))))

      (loop :for override :being :the :hash-values :of overrides-table :do
        (let ((super-slot (lookup-super override)))
          (unless super-slot
            (error "Slot override doesn't name a superclass slot: ~A" (direct-slot-name override)))
          (push (make-effective override super-slot) effective-slots)))

      (dolist (direct direct-slots)
        (let ((super-slot (lookup-super direct)))
          (when super-slot
            (error "Direct slot has the same name as a superclass slot: ~A" (direct-slot-name direct)))
          (push (make-effective direct nil) effective-slots)))

      (when superclass-slots
        (loop :for slot :being :the :hash-values :of superclass-slots :do
          (unless (gethash (symbol-name (effective-slot-name slot)) overrides-table)
            (push (make-effective nil slot) effective-slots))))

      effective-slots)))

(defmethod shared-initialize ((struct-class struct-class) slots &rest initargs
                              &key ((:direct-slots direct-slots-arg) nil direct-slots-p)
                                ((:slot-overrides slot-overrides-arg) nil slot-overrides-p))
  (setf initargs (initargs-remove initargs #(:direct-slots :slot-overrides :superclass)))

  (prog1
      (apply #'call-next-method struct-class slots initargs)
    (with-slots
          (direct-slots effective-slots slot-overrides
           direct-slots-table effective-slots-table slot-overrides-table)
        struct-class

      (labels
          ((make-slot (class form)
             (when (symbolp form)
               (setf form (list form)))
             (destructuring-bind (name &optional initform &rest initargs) form
               (apply 'make-instance class :name name
                                           :initform initform
                                           :reader (intern-conc (struct-class-package struct-class)
                                                                (struct-class-conc-name struct-class)
                                                                name)
                                           initargs))))

        (when direct-slots-p
          (let ((direct-slot-class (direct-slot-definition-class struct-class)))
            (setf direct-slots (mapcar (lambda (form) (make-slot direct-slot-class form))
                                       direct-slots-arg))))

        (when slot-overrides-p
          (let ((override-slot-class (direct-slot-definition-class (struct-class-superclass struct-class))))
            (setf slot-overrides (mapcar (lambda (form) (make-slot override-slot-class form))
                                         slot-overrides-arg)))))

      (setf direct-slots-table (make-slot-table 'direct-slot-name direct-slots))
      (setf slot-overrides-table (make-slot-table 'direct-slot-name slot-overrides))

      (setf effective-slots (compute-effective-slots struct-class))
      (setf effective-slots-table (make-slot-table 'effective-slot-name effective-slots)))))

(defvar *struct-classes* (make-hash-table :test 'eq))

(defun find-struct-class (name &key error-p)
  (or (nth-value 0 (gethash name *struct-classes*))
      (when error-p
        (error "No structure metaobject for name ~A" name))))

(defun (setf find-struct-class) (value name &key error-p)
  (declare (ignore error-p))
  (setf (gethash name *struct-classes*) value))

(defun initargs-remove (initargs keys)
  (loop :while initargs
        :for key = (pop initargs)
        :for value = (pop initargs)
        :nconc (unless (position key keys :test 'eq)
                 (list key value))))

(defun normalize-option (name option-name option-args)
  (case option-name
    (:copier
     (destructuring-bind (&optional (copier-name (intern-conc *package* "COPY-" name))) option-args
       (list option-name copier-name)))

    (:conc-name
     (destructuring-bind (&optional conc-name) option-args
       (list option-name conc-name)))

    (:constructor
     (destructuring-bind (&optional (constructor-name (intern-conc *package* "MAKE-" name))
                            (arg-list nil arg-list-p))
         option-args
       (list option-name (if arg-list-p
                             (list constructor-name arg-list)
                             constructor-name))))

    (:predicate
     (destructuring-bind (&optional (predicate-name (intern-conc *package* name "-P"))) option-args
       (list option-name predicate-name)))

    (:include
     (destructuring-bind (include-name &rest slot-overrides) option-args
       (list :superclass include-name
             :slot-overrides slot-overrides)))

    (otherwise
     (list option-name option-args))))

(defun normalize-options (name options)
  (let* (constructors
         (seen-options (make-hash-table :test 'eq))
         result)
    (labels
        ((handle (option)
           (destructuring-bind (option-name &rest option-args) (if (symbolp option)
                                                                   (list option)
                                                                   option)
             (let ((seen-p (gethash option-name seen-options)))
               (setf (gethash option-name seen-options) t)
               (cond
                 ((eq option-name :constructor)
                  (push (second (normalize-option name option-name option-args))
                        constructors)
                  nil)
                 ((not seen-p)
                  (normalize-option name option-name option-args))
                 (t
                  (error "Option ~A was specified more than once" option-name))))))

         (default (option-name &optional (value nil value-p))
           (unless (gethash option-name seen-options)
             (if value-p
                 (list option-name value)
                 (handle option-name)))))

      (setf result (mapcan #'handle options))
      (default :constructor)
      (append (list :constructors constructors)
              (default :copier)
              (default :conc-name (intern-conc nil name "-"))
              (default :predicate)
              (default :metaclass 'struct-class)
              result))))

(defun make-struct-metaobject (name &rest args
                               &key ((:metaclass input-metaclass))
                                 ((:superclass superclass-name))
                               &allow-other-keys)
  (let* ((superclass (when superclass-name
                       (find-struct-class superclass-name :error-p t)))
         (resolved-metaclass
           (cond
             ((and input-metaclass superclass)
              (unless (subtypep input-metaclass (class-of superclass))
                (error "The given metaclass isn't a subclass of the :include'd struct's metaclass"))
              input-metaclass)
             ((and input-metaclass (car input-metaclass))
              (car input-metaclass))
             (superclass
              (class-of superclass))
             (t
              (find-class 'struct-class))))
         (fixed-args
           (initargs-remove args #(:metaclass :superclass))))

    (unless (subtypep resolved-metaclass 'struct-class)
      (error "Cannot create a structure with metaclass that doesn't inherit from STRUCT-CLASS"))

    (setf fixed-args (list* :superclass superclass fixed-args))
    (setf fixed-args (list* :name name fixed-args))

    (apply 'make-instance resolved-metaclass fixed-args)))

(defgeneric slot-definition-form (slot))

(defmethod slot-definition-form ((slot direct-slot))
  `(,(direct-slot-name slot) ,(direct-slot-initform slot)
    ,@(unless (eq :unspecific (direct-slot-read-only slot))
        `(:read-only ,(direct-slot-read-only slot)))
    ,@(unless (eq :unspecific (direct-slot-type slot))
        `(:type ,(direct-slot-type slot)))))

(defgeneric struct-definition-options (struct))

(defmethod struct-definition-options ((struct struct-class))
  `((:copier ,(struct-class-copier struct))
    (:predicate ,(struct-class-predicate struct))
    (:conc-name ,(struct-class-conc-name struct))
    ,@(when (struct-class-superclass struct)
        `((:include ,(struct-class-name (struct-class-superclass struct))
                    ,@(mapcar 'slot-definition-form (struct-class-slot-overrides struct)))))
    ,@(mapcar (lambda (c) `(:constructor ,@(if (symbolp c) (list c) c)))
              (struct-class-constructors struct))))

(defgeneric struct-definition-form (struct))

(defmethod struct-definition-form ((struct struct-class))
  `(progn
     (defstruct (,(struct-class-name struct)
                 ,@(struct-definition-options struct))
       ,@(mapcar 'slot-definition-form (struct-class-direct-slots struct)))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (find-struct-class ',(struct-class-name struct)) ,struct))
     ',(struct-class-name struct)))

(defmacro define-struct (name-and-options &body slots)
  (let* ((name (if (consp name-and-options)
                   (car name-and-options)
                   name-and-options))
         (options (when (consp name-and-options)
                    (cdr name-and-options)))
         (normalized-options (normalize-options name options))
         (metaobject (apply 'make-struct-metaobject name :direct-slots slots normalized-options)))
    (struct-definition-form metaobject)))
