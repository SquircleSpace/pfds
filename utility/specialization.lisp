;; Copyright 2020 Ada Avery
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

(uiop:define-package :pfds.shcl.io/utility/specialization
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/utility/misc #:get+ #:fully-expand)
  (:import-from :alexandria)
  (:export
   #:define-specializable-function
   #:mutually-recursive-specializable-functions
   #:specialize))
(in-package :pfds.shcl.io/utility/specialization)

(defclass specialization-record ()
  ((%name
    :initarg :name
    :initform (error "name is required"))
   (%body
    :initarg :body
    :initform (error "body is required"))
   (%lambda-list
    :initarg :lambda-list
    :initform (error "lambda-list is required"))
   (%interfaces
    :initarg :interfaces
    :initform (error "interfaces is required"))
   (%table
    :initform (make-hash-table :test #'equal))
   (%dependencies
    :initarg :dependencies
    :initform (error "dependencies is required"))))

(defmethod make-load-form ((object specialization-record) &optional env)
  (make-load-form-saving-slots object :environment env))

(defun make-specialization-defun (specialization-name fname interfaces interface-arguments lambda-list body)
  (let ((base-name
          (etypecase fname
            (symbol fname)
            (list
             (unless (cdr fname)
               (error "Function name is invalid: ~W" fname))
             (second fname)))))
    (multiple-value-bind
          (remaining-forms declarations doc-string)
        (alexandria:parse-body body :documentation t)
      `(symbol-macrolet
           ,(loop :for name :in interfaces :for value :in interface-arguments
                  :collect (list name value))
         (defun ,specialization-name ,lambda-list
           ,@(when doc-string `(,doc-string))
           ,@declarations
           (block ,base-name
             ,@remaining-forms))))))

(defun specialization-record (function-name)
  (get+ function-name 'specializations))

(defun (setf specialization-record) (new-value function-name)
  (setf (get+ function-name 'specializations) new-value))

(defun ensure-specialization-record (function-name interfaces lambda-list body dependencies)
  (setf dependencies (remove (cons function-name interfaces) dependencies :test #'equal))

  (let ((existing (specialization-record function-name)))
    (if (not existing)
        (setf (specialization-record function-name)
              (make-instance 'specialization-record
                             :name function-name
                             :interfaces interfaces
                             :lambda-list lambda-list
                             :body body
                             :dependencies dependencies))
        (progn
          (reinitialize-instance existing
                                 :name function-name
                                 :interfaces interfaces
                                 :lambda-list lambda-list
                                 :body body
                                 :dependencies dependencies)
          (when (plusp (hash-table-count (slot-value existing '%table)))
            (warn "~W has specializations that are being lost" function-name))
          (setf (slot-value existing '%table) (make-hash-table :test #'equal))
          existing))))

(defun find-specialization (function-name interface-arguments)
  (let* ((record (specialization-record function-name))
         (symbol (when record
                   (gethash interface-arguments (slot-value record '%table)))))
    symbol))

(defun specialization-invocation (function-name interface-arguments other-args)
  (let ((symbol (find-specialization function-name interface-arguments)))
    (when symbol
      `(,symbol ,@other-args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compute-specializable-dependencies (interfaces body)
    ;; We're going to do a VERY bad code walk to look for places that
    ;; might be calls to other specializable functions.  Basically,
    ;; any time we see something in the car position followed by some
    ;; number of arguments from the INTERFACES set, we'll say that
    ;; function is depended on by this one.
    (let ((unique-calls (make-hash-table :test 'equal)))
      (labels
          ((compatible-args (call)
             (loop :with target = (car call)
                   :with record = (specialization-record target)
                   :with max-specialization-args = (length (slot-value record '%interfaces))
                   :with remaining = (cdr call)
                   :for i :below max-specialization-args
                   :while (and (consp remaining)
                               (position (car remaining) interfaces))
                   :collect (pop remaining)))

           (walk (list)
             (unless (consp list)
               (return-from walk))

             (unless (and (symbolp (car list))
                          (specialization-record (car list)))
               (dolist (form list)
                 (walk form))
               (return-from walk))

             (let ((args (compatible-args list)))
               (when args
                 (setf (gethash (cons (car list) args) unique-calls)
                       t)))))

        (dolist (form body)
          (walk form)))

      (loop :for key :being :the :hash-keys :of unique-calls
            :collect key))))

(defmacro define-specializable-function (name (&rest interfaces) lambda-list &body body)
  (let ((interface-args (mapcar (lambda (x) (declare (ignore x)) (gensym "INTERFACE")) interfaces))
        (whole (gensym "WHOLE"))
        (rest-args (gensym "REST-ARGS")))
    `(progn
       (define-compiler-macro ,name (&whole ,whole ,@interface-args &rest ,rest-args)
         (or (specialization-invocation ',name (list ,@interface-args) ,rest-args)
             ,whole))
       (defun ,name (,@interfaces ,@lambda-list)
         ,@body)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (ensure-specialization-record ',name ',interfaces ',lambda-list ',body
                                       ',(compute-specializable-dependencies interfaces body)))
       ',name)))

;; Compiler macros need to be defined before the first call or you get
;; a warning.  That's actually harmless in the case of specializable
;; functions.  The compiler macro won't have any effect until
;; specializations are defined, and that won't happen until after the
;; mutually recursive functions are defined.  Still, warnings suck.
;; This macro just rearranges forms to ensure that the warning doesn't
;; happen.
(defmacro mutually-recursive-specializable-functions (&body body &environment env)
  ;; This technically isn't good enough because the detected
  ;; dependencies aren't preserved.  Good enough for now.
  (let (pre-forms
        post-forms)
    (labels
        ((filter (list function)
           (loop :for item :in list
                 :when (funcall function item)
                   :collect item))
         (pre-form-p (form)
           (and (consp form)
                (or (eq 'define-compiler-macro (car form))
                    (eq 'eval-when (car form)))))
         (extract-forms (form)
           (let* ((expanded (macroexpand-1 form env))
                  (pre (filter expanded #'pre-form-p))
                  (post (filter expanded (lambda (f) (not (pre-form-p f))))))
             (push `(progn ,@pre) pre-forms)
             (push post post-forms)))
         (process-form (form)
           (if (and (consp form)
                    (eq (car form) 'define-specializable-function))
               (extract-forms form)
               (push form post-forms))))
      (loop :for form :in body :do (process-form form))
      `(progn
         ,@(nreverse pre-forms)
         ,@(nreverse post-forms)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %recursive-specialize (function-name interface-arguments environment seen-table)
    (let ((target (cons function-name interface-arguments)))
      (when (nth-value 1 (gethash (cons function-name interface-arguments) seen-table))
        (return-from %recursive-specialize))

      (setf (gethash target seen-table)
            `(specialize-1 ,function-name ,@interface-arguments)))

    (let* ((record (or (specialization-record function-name)
                       (error "Couldn't find a specializable function named ~W" function-name)))
           (dependencies (slot-value record '%dependencies))
           (argument-mapping (make-hash-table :test 'eql)))
      (loop :for argument-name :in (slot-value record '%interfaces)
            :for argument :in interface-arguments
            :do (setf (gethash argument-name argument-mapping) argument))

      (labels
          ((build-args (dependency-arguments)
             (loop :for argument-name :in dependency-arguments
                   :for value = (gethash argument-name argument-mapping)
                   :while value :collect value)))
        (loop :for dependency :in dependencies
              :for mapped-arguments = (build-args (cdr dependency))
              :when mapped-arguments :do
                (%recursive-specialize (car dependency) mapped-arguments environment seen-table)))))

  (defun recursive-specialize (name interface-arguments &optional environment)
    (let ((table (make-hash-table :test #'equal)))
      (%recursive-specialize name interface-arguments environment table)
      `(progn
         ,@(loop :for form :being :the :hash-values :of table
                 :collect form)
         ',name))))

(defun record-specialization (name arguments target)
  (let* ((record (specialization-record name))
         (table (slot-value record '%table)))
      (setf (gethash arguments table) target)))

(defmacro specialize (name &rest interface-arguments &environment env)
  (recursive-specialize name interface-arguments env))

(defmacro specialize-1 (name &rest interface-arguments &environment env)
  (dolist (arg interface-arguments)
    (unless (or (constantp arg env)
                (constantp (fully-expand arg env) env))
      (error "Interface argument isn't constant: ~W" arg)))
  (let* ((specialization-record (or (specialization-record name) (error "~W isn't specializable" name)))
         (provided-length (length interface-arguments))
         (expected-length (length (slot-value specialization-record '%interfaces)))
         (specialization-name (gensym (format nil "~A-~A" name interface-arguments)))
         (specialization-defun (make-specialization-defun
                                specialization-name
                                (slot-value specialization-record '%name)
                                (slot-value specialization-record '%interfaces)
                                interface-arguments
                                (slot-value specialization-record '%lambda-list)
                                (slot-value specialization-record '%body))))

    (unless (equal provided-length expected-length)
      (error "Incorrect number of interface arguments.  Got ~A, expected ~A" provided-length expected-length))
    `(progn
       ,specialization-defun
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (record-specialization
          ',name
          ',interface-arguments
          ',specialization-name))
       ',name)))
