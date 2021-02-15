;; Copyright 2021 Ada Avery
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

(uiop:define-package :pfds.shcl.io/tests/test-interface
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:import-from :pfds.shcl.io/utility/misc
   #:intern-conc
   #:string-starts-with-p
   #:string-ends-with-p)
  (:import-from :pfds.shcl.io/utility/forwarding
   #:make-forwarding-defun)
  (:import-from :pfds.shcl.io/implementation/interface)
  (:export
   #:*interface*
   #:^graphviz))
;; exports will be done dynamically
(in-package :pfds.shcl.io/tests/test-interface)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun interface-class-name-p (sym)
    (and (string-starts-with-p (symbol-name sym) "<<")
         (string-ends-with-p (symbol-name sym) ">>")
         (find-class sym nil)))

  (defun exported-symbol-p (sym package)
    (multiple-value-bind (other-sym exposure) (find-symbol (symbol-name sym) package)
      (and (eq other-sym sym)
           (eq exposure :external))))

  (defun bridge-function (original-function-name)
    (let* ((our-function-name (intern-conc
                               :pfds.shcl.io/tests/test-interface
                               "^"
                               (symbol-name original-function-name)))
           (lambda-list (interface-function-lambda-list original-function-name)))
      `(progn
         (export ',our-function-name :pfds.shcl.io/tests/test-interface)
         ,(make-forwarding-defun
           our-function-name
           lambda-list
           `(interface-get *interface* ',original-function-name))))))

(defmacro forms ()
  (let ((classes (loop
                   :for symbol :being :the :external-symbols :of :pfds.shcl.io/implementation/interface
                   :when (interface-class-name-p symbol)
                     :collect symbol))
        (function-table (make-hash-table :test 'equal))
        functions)
    (loop :for class-name :in classes :do
      (loop :for pair :in (interface-functions class-name)
            :for function-name = (car pair) :do
              (setf (gethash function-name function-table) t)))
    (setf functions
          (loop :for function-name :being :the :hash-keys :of function-table
                :collect function-name))

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop :for class-name :in classes
               :collect `(progn
                           (import ',class-name :pfds.shcl.io/tests/test-interface)
                           (export ',class-name :pfds.shcl.io/tests/test-interface)))
       ,@(loop :for function :in functions
               :collect (bridge-function function)))))

(defvar *interface*)

(forms)

(defun ^graphviz (object &optional (stream *standard-output*))
  (pfds.shcl.io/implementation/interface:graphviz object stream *interface*))
