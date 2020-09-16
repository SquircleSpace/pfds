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

(defpackage :pfds.shcl.io/tests/main
  (:use :common-lisp)
  (:use :pfds.shcl.io/tests/common)
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/compare #:compare)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc)
  (:import-from :pfds.shcl.io/utility/structure-mop
   #:find-struct-class #:struct-class-constructors
   #:struct-class-subclasses #:struct-class-name
   #:make-struct-metaobject)
  (:import-from :pfds.shcl.io/tests/debugable #:test-debugable)
  (:import-from :pfds.shcl.io/tests/comparable #:test-comparable)
  (:import-from :pfds.shcl.io/tests/collection #:test-collection)
  (:import-from :pfds.shcl.io/tests/ordered-collection #:test-ordered-collection)
  (:import-from :pfds.shcl.io/tests/stack #:test-stack)
  (:import-from :pfds.shcl.io/tests/set #:test-set)
  (:import-from :pfds.shcl.io/tests/ordered-map #:test-ordered-map)
  (:import-from :pfds.shcl.io/tests/queue #:test-queue)
  (:import-from :pfds.shcl.io/tests/priority-queue #:test-priority-queue)
  (:import-from :pfds.shcl.io/tests/compare #:test-compare)
  (:import-from :pfds.shcl.io/implementation/list)
  (:import-from :pfds.shcl.io/implementation/bankers-queue #:bankers-queue)
  (:import-from :pfds.shcl.io/implementation/batched-queue #:batched-queue)
  (:import-from :pfds.shcl.io/implementation/binomial-heap #:binomial-heap)
  (:import-from :pfds.shcl.io/implementation/leftist-heap #:leftist-heap)
  (:import-from :pfds.shcl.io/implementation/pairing-heap #:pairing-heap)
  (:import-from :pfds.shcl.io/implementation/splay-tree #:splay-heap)
  (:import-from :pfds.shcl.io/implementation/batched-deque #:batched-deque)
  (:import-from :pfds.shcl.io/implementation/lazy-list #:lazy-list)
  (:import-from :pfds.shcl.io/implementation/persistent-vector #:persistent-vector)
  (:import-from :pfds.shcl.io/implementation/weight-balanced-tree
   #:weight-balanced-sequence #:weight-balanced-set #:weight-balanced-map)
  (:import-from :pfds.shcl.io/implementation/red-black-tree
   #:red-black-set #:red-black-map)
  (:import-from :prove #:*suite* #:suite #:finalize #:skip)
  (:export #:run-tests #:main))
(in-package :pfds.shcl.io/tests/main)

(defvar *list-metaclass-shim*
  (make-struct-metaobject 'list :constructors nil))

(defun make-cons-shim ()
  (cons nil nil))

(defvar *cons-metaclass-shim*
  (make-struct-metaobject 'cons :constructors '(make-cons) :superclass *list-metaclass-shim*))

(defun make-null ()
  nil)

(defvar *null-metaclass-shim*
  (make-struct-metaobject 'null :constructors '(make-null) :superclass *list-metaclass-shim*))

(pfds.shcl.io/utility/structure-mop::register-subclass *list-metaclass-shim* *cons-metaclass-shim*)
(pfds.shcl.io/utility/structure-mop::register-subclass *list-metaclass-shim* *null-metaclass-shim*)

(defmethod compare-objects ((left null) (right null))
  ;; This method should never run.  compare should early return when
  ;; given eql objects!  Unfortunately, without this we'll incorrectly
  ;; conclude that the compare-objects GF doesn't have methods for the
  ;; list type.
  :equal)

(defun shimed-find-struct-class (name &key error-p)
  (case name
    (list *list-metaclass-shim*)
    (cons *cons-metaclass-shim*)
    (null *null-metaclass-shim*)
    (otherwise
     (find-struct-class name :error-p error-p))))

(defun has-method (function-name class-name)
  (let* ((function (symbol-function function-name))
         (class (find-class class-name))
         (methods (closer-mop:generic-function-methods function)))
    (dolist (method methods)
      (let ((specializers (closer-mop:method-specializers method)))
        (when (find class specializers)
          (return-from has-method t))))

    ;; Before we give up, let's see if its an ADT.  Maybe all the
    ;; subclasses conform.
    (let ((struct-class (shimed-find-struct-class class-name)))
      (unless struct-class
        ;; Guess its not an ADT!
        (return-from has-method nil))

      ;; If its possible to construct this type, then its not an ADT.
      (when (struct-class-constructors struct-class)
        (return-from has-method nil))

      ;; Cool, no constructors.  Let's see if all the subclasses
      ;; conform.
      (let ((subclasses (struct-class-subclasses struct-class)))
        (unless subclasses
          ;; Wat?
          (return-from has-method nil))

        (dolist (subclass subclasses)
          (unless (has-method function-name (struct-class-name subclass))
            ;; Shoot, this subclass doesn't conform.  Oh well.
            (return-from has-method nil))))

      ;; If we made it here, then all the subclasses have methods.
      ;; We're good!
      t)))

(defun check-interface-conformance (class-name interface-name)
  (let (missing-methods)
    (dolist (function (interface-functions interface-name :include-inherited-functions nil))
      (let ((function-name (car function))
            (strength (cdr function)))
        (when (and (eq strength :required)
                   (not (has-method function-name class-name)))
          (push function-name missing-methods))))
    (prove:is missing-methods nil
              (format nil "~A conforms to ~A" class-name interface-name))
    (null missing-methods)))

(defparameter *test-environments*
  (labels
      ((env (class-name &rest initargs)
         (apply 'make-test-environment class-name initargs)))
    (list
     ;; Queue
     (env 'bankers-queue)
     (env 'batched-queue)
     ;; Priority Queue
     (env 'binomial-heap)
     (env 'leftist-heap :bias :weight)
     (env 'leftist-heap :bias :height)
     (env 'pairing-heap)
     (env 'splay-heap)
     ;; Deque
     (env 'batched-deque)
     ;; Sequence
     (env 'lazy-list)
     (env 'list)
     (env 'persistent-vector)
     (env 'weight-balanced-sequence)
     ;; Set
     (env 'red-black-set)
     (env 'weight-balanced-set)
     ;; Map
     (env 'weight-balanced-map)
     (env 'red-black-map))))

(defparameter *interface-tests*
  (alexandria:alist-hash-table
   '((debugable . test-debugable)
     (comparable . test-comparable)
     (collection . test-collection)
     (ordered-collection . test-ordered-collection)
     ;; (indexed-collection . test-indexed-collection)
     (ordered-map . test-ordered-map)
     (set . test-set)
     (stack . test-stack)
     (queue . test-queue)
     ;; (deque . test-deque)
     (priority-queue . test-priority-queue)
     ;; (sequence . test-sequence)
     )))

(defun run-type-tests ()
  (let* ((interfaces (sort (copy-list (interfaces-implemented *name*)) #'string<)) ;; Establish stable ordering
         (visited-interfaces (make-hash-table))
         test-plan
         untestable-interfaces)

    (labels
        ;; Build a test plan that ensures more foundational tests
        ;; come first
        ((visit (interface)
           (when (gethash interface visited-interfaces)
             (return-from visit))
           (setf (gethash interface visited-interfaces) t)
           (let ((supers (interface-supers interface)))
             (dolist (super supers)
               (visit super)))
           (let ((test-fn (gethash interface *interface-tests*)))
             (if test-fn
                 (push (cons interface test-fn) test-plan)
                 (push interface untestable-interfaces)))))
      (dolist (interface interfaces)
        (visit interface)))

    (setf test-plan (nreverse test-plan))

    (dolist (test test-plan)
      (subtest* (symbol-name (car test))
        (if (check-interface-conformance *name* (car test))
            (funcall (cdr test))
            (skip 1 "No point testing ~A interface if conformance is incomplete" (car test)))))

    (when untestable-interfaces
      (subtest* "Other interfaces"
        (dolist (interface untestable-interfaces)
          (check-interface-conformance *name* interface))))))

(defun test-types (&optional (environments *test-environments*))
  (dolist (environment environments)
    (with-test-environment environment
      (subtest* (symbol-name *name*)
        (run-type-tests)))))

(defun test-type (name &rest initargs)
  (with-test-environment (apply 'make-test-environment name initargs)
    (run-type-tests)))

(defun all-tests ()
  (let ((*suite* (make-instance 'suite :plan nil)))
    (test-compare)
    (test-types)
    (finalize)))

(defun main ()
  (if (all-tests)
      (uiop:quit 0)
      (uiop:quit 1)))
