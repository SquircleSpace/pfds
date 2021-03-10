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

(uiop:define-package :pfds.shcl.io/utility/interface
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add
   #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc #:get+ #:fully-expand)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare #:compare-objects)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/forwarding
   #:make-forwarding-lambda #:make-forwarding-method)
  (:import-from :alexandria)
  (:export
   #:define-interface
   #:define-interface-instance
   #:define-simple-interface-instance
   #:interface-get
   #:interface-get-function
   #:interface-get-subinterface

   #:interface-functions
   #:interface-superclasses
   #:interface-function-lambda-list
   #:interface-instance-debug-name

   #:define-interface-function-invoker
   #:define-interface-function
   #:single-specializer
   #:double-specializer
   #:define-interface-methods
   #:declaim-signature
   #:proclaim-signature))
(in-package :pfds.shcl.io/utility/interface)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %do-interface-inheritance-tree-f (interface-stack visited function)
    (let ((interface-name (car interface-stack)))
      (when (gethash interface-name visited)
        (return-from %do-interface-inheritance-tree-f))

      (when (position interface-name (cdr interface-stack))
        (error "Cyclic inheritance detected: ~W" interface-stack))

      (let ((supers (interface-superclasses interface-name)))
        (dolist (super supers)
          (%do-interface-inheritance-tree-f (cons super interface-stack) visited function))
        (funcall function interface-name)
        (setf (gethash interface-name visited) t))))

  (defun do-interface-inheritance-tree-f (interface-name function)
    (%do-interface-inheritance-tree-f (list interface-name) (make-hash-table) function))

  (defmacro do-interface-inheritance-tree ((interface-name seed-interface) &body body)
    `(do-interface-inheritance-tree-f ,seed-interface (lambda (,interface-name) ,@body)))

  (defgeneric interface-superclasses (interface))

  (defmethod interface-superclasses ((interface standard-class))
    (interface-superclasses (class-name interface)))

  (defgeneric interface-optional-functions (interface))

  (defmethod interface-optional-functions ((interface standard-class))
    (interface-optional-functions (class-name interface)))

  (defgeneric interface-required-functions (interface))

  (defmethod interface-required-functions ((interface standard-class))
    (interface-required-functions (class-name interface)))

  (defun interface-functions (interface &key (include-inherited-functions t))
    (let ((visited (make-hash-table)))
      (labels
          ((visit-interface (interface)
             (let ((required-functions (interface-required-functions interface))
                   (optional-functions (interface-optional-functions interface)))
               (dolist (function optional-functions)
                 (setf (gethash function visited) :optional))
               (dolist (function required-functions)
                 (let ((value (gethash function visited)))
                   (ecase value
                     ((:required :optional))
                     ((nil)
                      (setf (gethash function visited) :required))))))))
        (if include-inherited-functions
            (do-interface-inheritance-tree (interface interface)
              (visit-interface interface))
            (visit-interface interface)))
      (loop :for function-name :being :the :hash-keys :of visited
              :using (:hash-value strength) :collect
                                            (cons function-name strength))))

  (defclass interface ()
    ((%debug-name
      :type symbol
      :initarg :debug-name
      :initform nil)))

  (defun interface-instance-debug-name (interface)
    (slot-value interface '%debug-name))

  (defmethod make-load-form ((interface interface) &optional env)
    (make-load-form-saving-slots interface :environment env))

  (defmethod print-object ((interface interface) stream)
    (print-unreadable-object (interface stream :type t :identity t)
      (let ((name (interface-instance-debug-name interface)))
        (when name
          (format stream "~W" name)))))

  (defmethod interface-superclasses ((interface interface))
    nil)

  (defmethod interface-required-functions ((interface interface))
    nil)

  (defmethod interface-optional-functions ((interface interface))
    nil))

(defmacro define-interface (name superclasses &body clauses)
  (let (required-functions
        optional-function-pairs
        optional-function-names
        slots
        (instance (gensym "INSTANCE")))
    (loop :for thing :in clauses :do
      (etypecase thing
        (symbol
         (push thing required-functions))
        (cons
         (ecase (car thing)
           (:required
            (destructuring-bind (symbol) (cdr thing)
              (check-type symbol symbol)
              (push symbol required-functions)))
           (:optional
            (destructuring-bind (symbol &optional (default-value symbol)) (cdr thing)
              (check-type symbol symbol)
              (check-type default-value symbol)
              (push (cons symbol default-value) optional-function-pairs)
              (push symbol optional-function-names)))))))
    (dolist (function required-functions)
      (push `(,function
              :initform (error "~W is a required function for this interface" ',function)
              :initarg ,function)
            slots))
    (dolist (pair optional-function-pairs)
      (push `(,(car pair)
              :initform ',(cdr pair)
              :initarg ,(car pair))
            slots))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name (,@superclasses interface)
         (,@slots))
       (defmethod interface-superclasses ((,instance ,name))
         ',superclasses)
       (defmethod interface-superclasses ((,instance (eql ',name)))
         ',superclasses)
       (defmethod interface-required-functions ((,instance ,name))
         ',required-functions)
       (defmethod interface-required-functions ((,instance (eql ',name)))
         ',required-functions)
       (defmethod interface-optional-functions ((,instance ,name))
         ',optional-function-names)
       (defmethod interface-optional-functions ((,instance (eql ',name)))
         ',optional-function-names)
       ',name)))

(defmacro define-interface-instance (instance-name interface-name &rest initargs)
  (let ((constant-sym (gensym (symbol-name instance-name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defconstant ,constant-sym
         (if (boundp ',constant-sym)
             (symbol-value ',constant-sym)
             (make-instance ',interface-name ,@initargs :debug-name ',instance-name)))
       (defmacro ,constant-sym ()
         ',constant-sym)
       (define-symbol-macro ,instance-name (,constant-sym)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun quote-p (form)
    (and (consp form)
         (eq 'quote (car form))))

  (defun de-quote (form)
    (if (quote-p form)
        (second form)
        (error "Not a quote: ~W" form))))

(defmacro define-simple-interface-instance (name interface-name conc-name &rest overrides)
  (let ((interface-table (make-hash-table))
        initarg-elements)
    (loop :while overrides
          :for initarg = (pop overrides)
          :for initarg-munged = (if (keywordp initarg) `',initarg initarg)
          :for value = (if overrides (pop overrides) (error "Odd number of initargs provided"))
          :for de-quoted = (if (quote-p initarg-munged)
                               (de-quote initarg-munged)
                               (error "Received an unsupported form for an initarg: ~W" initarg))
          :do
             (progn
               (unless (symbolp de-quoted)
                 (error "Received an unsupported form for an initarg: ~W" initarg))
               (setf (gethash de-quoted interface-table) t)
               (push (list initarg value) initarg-elements)))

    (loop :for pair :in (interface-functions interface-name)
          :for sym = (car pair) :for strength = (cdr pair) :do
            (unless (or (nth-value 1 (gethash sym interface-table))
                        (eq strength :optional))
              (setf (gethash sym interface-table) t)
              (push (list `',sym `',(intern-conc (symbol-package conc-name) conc-name sym))
                    initarg-elements)))

    (setf initarg-elements (nreverse initarg-elements))

    `(define-interface-instance ,name ,interface-name
       ,@(loop :for element :in initarg-elements :nconc element))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun maybe-de-quote (form)
    (unless (quote-p form)
      (return-from maybe-de-quote
        form))

    (let ((quoted (de-quote form)))
      ;; Objects other than a conses and symbols are self-evaluating
      ;; and can be safely de-quoted
      (if (or (symbolp quoted)
              (consp quoted))
          form
          quoted)))

  (declaim (inline interface-has-function-p))
  (defun interface-has-function-p (interface function-name)
    (slot-exists-p interface function-name))

  (defun interface-get-form (interface identifier function-p env)
    (setf interface (macroexpand interface env))
    (when (and (symbolp interface)
               (constantp interface env))
      ;; Ignore errors in case the symbol doesn't have a value yet.
      ;; If it does have a value then we can safely assume it won't
      ;; change between now and runtime.
      (ignore-errors
       (setf interface (symbol-value interface))))

    (setf identifier (macroexpand identifier env))

    (labels
        ((maybe-quote (form)
           (if function-p
               `',form
               form)))
      (cond
        ((and (typep interface 'interface)
              (quote-p identifier))
         ;; Objects inheriting from INTERFACE are assumed to be
         ;; immutable.  There's no reason to wait to perform the lookup
         ;; at runtime!  Might as well do it now!
         (maybe-quote (slot-value interface (de-quote identifier))))

        ((and (constantp interface env)
              (constantp identifier env))
         ;; This is an unusual case!  Typically, when we're given a
         ;; constant, it will be covered by the above logic (i.e. a
         ;; direct reference to a constant symbol).  This would be
         ;; something fancy that CONSTANTP is able to recognize that we
         ;; aren't (e.g. (progn +some-constant+)).  That's unlikely to
         ;; happen with human-authored code, but it could definitely
         ;; happen with macro-generated code.

         ;; There isn't a nice way to evaluate constant forms.  We could
         ;; just EVAL it... but a strict reading of the standard doesn't
         ;; allow us to do that.  EVAL runs with a nil lexical
         ;; environment, but CONSTANTP is allowed to depend on
         ;; environment when making its determination.  Our only option
         ;; is to defer the evaluation to runtime.
         #-sbcl
         (if function-p
             `(load-time-value (slot-value ,interface ,identifier) t)
             `(load-time-value (symbol-value (slot-value ,interface ,identifier)) t))

         ;; SBCL has a super handy function for evaluating a constant
         ;; form at compile time.  Might as well use it!
         #+sbcl
         (maybe-quote (slot-value (sb-int:constant-form-value interface env)
                                  (sb-int:constant-form-value identifier env))))

        (t
         (if function-p
             `(slot-value ,interface ,identifier)
             `(symbol-value (slot-value ,interface ,identifier))))))))

(defmacro interface-get-function (interface function-name &environment env)
  (interface-get-form interface function-name t env))

(defmacro interface-get-subinterface (interface identifier &environment env)
  (interface-get-form interface identifier nil env))

(defun interface-get (interface identifier)
  (slot-value interface identifier))

(defconstant +interface-function-metadata+ '+interface-function-metadata+)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-immutable-structure interface-function-metadata
    (lambda-list nil :type list)
    (documentation nil :type (or string null)))

  (defmethod make-load-form ((o interface-function-metadata) &optional environment)
    (make-load-form-saving-slots o :environment environment)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun proclaim-signature (function-name lambda-list &key documentation)
    (check-type function-name symbol)
    (check-type lambda-list list)
    (check-type documentation (or string null))
    (setf (get+ function-name +interface-function-metadata+)
          (make-interface-function-metadata :lambda-list lambda-list
                                            :documentation documentation))
    function-name))

(defmacro declaim-signature (function-name lambda-list &key documentation)
  (check-type function-name symbol)
  (check-type lambda-list list)
  (check-type documentation (or string null))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (proclaim-signature ',function-name ',lambda-list
                         :documentation ,documentation)))

(defmethod documentation ((symbol symbol) (doc-type (eql 'interface-function)))
  (let ((metadata (get+ symbol +interface-function-metadata+)))
    (when metadata
      (interface-function-metadata-documentation metadata))))

(defmethod (setf documentation) (new-value (symbol symbol) (doc-type (eql 'interface-function)))
  (let ((old (or (get+ symbol +interface-function-metadata+)
                 (error "Symbol isn't known to be an interface function: ~W" symbol))))
    (setf (get+ symbol +interface-function-metadata+)
          (copy-interface-function-metadata old :documentation new-value))
    new-value))

(defun interface-function-lambda-list (function-name)
  (let ((metadata (get+ function-name +interface-function-metadata+)))
    (unless metadata
      (error "Unknown interface function: ~W" function-name))
    (interface-function-metadata-lambda-list metadata)))

(defmacro define-interface-function-invoker (wrapper-name interface-function-name)
  (let* ((metadata (get+ interface-function-name +interface-function-metadata+))
         (documentation (interface-function-metadata-documentation metadata))
         (lambda-list (interface-function-metadata-lambda-list metadata))
         (interface (make-symbol "<INTERFACE>"))
         (rest (gensym "REST"))
         (target (gensym "TARGET"))
         (env (gensym "ENV"))
         (forwarding-lambda (make-forwarding-lambda
                             lambda-list
                             `(interface-get-function ,interface ',interface-function-name)))
         (forwarding-lambda-list (second forwarding-lambda))
         (forwarding-body (cddr forwarding-lambda)))

    `(progn
       (declaim (inline ,wrapper-name))
       (defun ,wrapper-name (,interface ,@forwarding-lambda-list)
         ,@(when documentation `(,documentation))
         ,@forwarding-body)

       (define-compiler-macro ,wrapper-name (,interface &rest ,rest &environment ,env)
         ;; Use FUNCALL to prevent local shadowing.  Sadly, SBCL isn't
         ;; smart enough to inline a FUNCALL to an inline function
         ;; (aside from some functions that SBCL itself defines).
         ;; Since we macroexpand the target before we hand it to
         ;; FUNCALL, we make it more likely that compiler macros will
         ;; run, though!  Instead of seeing (FUNCALL (SOME-FORMS)) it
         ;; will often see (FUNCALL 'SOME-SYM).
         (let ((,target (macroexpand `(interface-get-function ,,interface ',',interface-function-name) ,env)))
           `(funcall ,,target ,@,rest))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun n-specializer (count generic-name class-name interface interface-function-name)
    (let ((lambda-list (interface-function-lambda-list interface-function-name)))
      (make-forwarding-method generic-name lambda-list `(interface-get-function ,interface ',interface-function-name)
                              :specializers (loop :for i :below count :collect class-name))))

  (defun single-specializer (generic-name class-name interface interface-function-name)
    (n-specializer 1 generic-name class-name interface interface-function-name))

  (defun double-specializer (generic-name class-name interface interface-function-name)
    (n-specializer 2 generic-name class-name interface interface-function-name)))

(defconstant +method-generator+ '+method-generator+)
(defconstant +generic-name+ '+generic-name+)

(defmacro define-interface-function (function-name lambda-list
                                     &key documentation
                                       (invoker (when (symbolp function-name)
                                                  (intern-conc *package* "I-" function-name)))
                                       (define-generic lambda-list)
                                       (generic-name (when define-generic
                                                       (intern-conc *package* "G-" function-name)))
                                       (method-generator 'single-specializer))
  (check-type function-name (or symbol list))
  (check-type invoker (or symbol list))
  (check-type generic-name (or symbol list))
  `(progn
     (declaim-signature ,function-name ,lambda-list :documentation ,documentation)
     ,(when invoker
        `(define-interface-function-invoker ,invoker ,function-name))
     ,(when define-generic
        `(defgeneric ,generic-name ,lambda-list
           (:documentation ,documentation)))
     ,(when generic-name
        `(setf (get+ ',function-name +generic-name+) ',generic-name))
     ,(when (and generic-name method-generator)
        `(setf (get+ ',function-name +method-generator+) ',method-generator))
     ',function-name))

(defmacro define-interface-methods (instance class-name &environment env)
  (let ((expanded (macroexpand instance env)))
    (unless (and (symbolp expanded)
                 (constantp expanded env))
      (error "Interface instance must be a simple constant: ~W" instance))
    (setf instance (symbol-value expanded)))
  `(progn
     ,@(loop :for interface-record :in (interface-functions instance)
             :for interface-function-name = (car interface-record)
             :for generator = (get+ interface-function-name +method-generator+)
             :for generic-name = (get+ interface-function-name +generic-name+)
             :when (and generator generic-name)
               :collect
               (funcall generator generic-name class-name instance interface-function-name))
     ',class-name))
