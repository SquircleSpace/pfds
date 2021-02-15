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

(uiop:define-package :pfds.shcl.io/tests/main
  (:use :common-lisp)
  (:use :pfds.shcl.io/tests/common)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/tests/test-interface)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects)
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
  ;; (:import-from :pfds.shcl.io/tests/set #:test-set)
  ;; (:import-from :pfds.shcl.io/tests/ordered-map #:test-ordered-map)
  (:import-from :pfds.shcl.io/tests/queue #:test-queue)
  (:import-from :pfds.shcl.io/tests/deque #:test-deque)
  (:import-from :pfds.shcl.io/tests/priority-queue #:test-priority-queue)
  (:import-from :pfds.shcl.io/tests/compare #:test-compare)
  (:import-from :pfds.shcl.io/implementation/list #:<list>)
  (:import-from :pfds.shcl.io/implementation/lazy-list #:<lazy-list>)
  (:import-from :pfds.shcl.io/implementation/bankers-queue #:<bankers-queue>)
  (:import-from :pfds.shcl.io/implementation/batched-queue #:<batched-queue>)
  (:import-from :pfds.shcl.io/implementation/binomial-heap #:<binomial-heap>)
  (:import-from :pfds.shcl.io/implementation/leftist-heap
   #:<weight-biased-leftist-heap> #:<height-biased-leftist-heap>)
  (:import-from :pfds.shcl.io/implementation/pairing-heap #:<pairing-heap>)
  ;; (:import-from :pfds.shcl.io/implementation/splay-tree #:splay-heap)
  (:import-from :pfds.shcl.io/implementation/batched-deque #:<batched-deque>)
  (:import-from :pfds.shcl.io/implementation/persistent-vector #:<persistent-vector>)
  ;; (:import-from :pfds.shcl.io/implementation/weight-balanced-tree
  ;;  #:weight-balanced-sequence #:weight-balanced-set #:weight-balanced-map)
  ;; (:import-from :pfds.shcl.io/implementation/red-black-tree
  ;;  #:red-black-set #:red-black-map)
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
  (let* ((function (fdefinition function-name))
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

(defun check-interface-conformance (class-name interface-instance)
  (let ((missing-methods
          (loop :for pair :in (interface-functions interface-instance)
                :for function-name = (car pair)
                :for strength = (cdr pair)
                :when (or (not (fboundp function-name))
                          (and (eq strength :required)
                               (typep (fdefinition function-name) 'generic-function)
                               (not (has-method function-name class-name))))
                  :collect function-name)))
    (prove:is missing-methods nil
              (format nil "~A conforms to ~A" class-name interface-instance))
    (null missing-methods)))

(defparameter *interface-symbols*
  '(<list>
    <lazy-list>
    <batched-queue>
    <batched-deque>
    <bankers-queue>
    <height-biased-leftist-heap>
    <weight-biased-leftist-heap>
    <pairing-heap>
    <binomial-heap>
    <persistent-vector>))

(defparameter *interface-tests*
  '((<<debugable>> . test-debugable)
    ((and <<debugable>> <<comparable>>) . test-comparable)
    (<<collection>> . test-collection)
    (<<ordered-collection>> . test-ordered-collection)
    ;; (<<indexed-collection>> . test-indexed-collection)
    ;; (<<ordered-map>> . test-ordered-map)
    ;; (<<set>> . test-set)
    (<<stack>> . test-stack)
    (<<queue>> . test-queue)
    (<<deque>> . test-deque)
    (<<priority-queue>> . test-priority-queue)
    ;; (<<sequence>> . test-sequence)
    ))

(defun run-interface-tests (interface-instance)
  (ensure-suite
    (loop :for record :in *interface-tests*
          :for type = (car record) :for function = (cdr record)
          :when (typep interface-instance type) :do
            (subtest* (symbol-name function)
              (let ((*interface* interface-instance))
                (funcall function))))))

(defun test-interfaces (&optional (interface-symbols *interface-symbols*))
  (ensure-suite
    (dolist (sym interface-symbols)
      (subtest* (symbol-name sym)
        ;; Macroexpand the symbol because we use symbol macros to make
        ;; interface instances redefineable
        (run-interface-tests (macroexpand sym))))))

(defun all-tests ()
  (ensure-suite
    (test-compare)
    (test-interfaces)))

(defun main ()
  (if (all-tests)
      (uiop:quit 0)
      (uiop:quit 1)))
