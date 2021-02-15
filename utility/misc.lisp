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

(uiop:define-package :pfds.shcl.io/utility/misc
  (:use :common-lisp)
  (:export
   #:intern-conc
   #:cassert
   #:get+
   #:compiler-macroexpand
   #:compiler-macroexpand-1
   #:fully-expand
   #:string-starts-with-p
   #:string-ends-with-p))
(in-package :pfds.shcl.io/utility/misc)

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

(defmacro cassert (condition &optional places datum &rest args)
  (let ((done (gensym "DONE")))
    `(block ,done
       (restart-case
           (assert ,condition ,places ,datum ,@args)
         (ignore ()
           (return-from ,done))))))

(defconstant +get-table+ '+get-table+)

(defun get+ (name indicator &optional default)
  (etypecase name
    (symbol
     (get name indicator default))
    (list
     (check-type (second name) symbol)
     (let* ((primary-name (second name))
            (get-table (or (get primary-name +get-table+)
                           (return-from get+ default))))
       (nth-value 0 (gethash name get-table default))))))

(defun (setf get+) (new-value name indicator &optional default)
  (declare (ignore default))
  (etypecase name
    (symbol
     (setf (get name indicator) new-value))
    (list
     (check-type (second name) symbol)
     (let* ((primary-name (second name))
            (get-table (or (get primary-name +get-table+)
                           (setf (get primary-name +get-table+)
                                 (make-hash-table :test #'equal)))))
       (setf (gethash name get-table) new-value)))))

(defun compiler-macroexpand-1 (form &optional environment)
  (let ((function (and (consp form)
                       (symbolp (car form))
                       (compiler-macro-function (car form) environment))))
    (unless function
      (return-from compiler-macroexpand-1
        (values form nil)))

    (let ((result (funcall *macroexpand-hook* function form environment)))
      (values result (not (eql form result))))))

(defun compiler-macroexpand (form &optional environment)
  (loop
    :with expanded-p = nil :do
      (multiple-value-bind (new-form expanded-this-time-p) (compiler-macroexpand-1 form environment)
        (unless expanded-this-time-p
          (return (values form expanded-p)))
        (setf expanded-p t)
        (setf form new-form))))

(defun fully-expand (form &optional environment)
  (multiple-value-bind
        (form macroexpanded)
      (macroexpand form environment)
    (multiple-value-bind
          (form compiler-macroexpanded)
        (compiler-macroexpand form environment)
      (values form macroexpanded compiler-macroexpanded))))

(defun string-starts-with-p (string prefix)
  (and (>= (length string)
           (length prefix))
       (string= string prefix :end1 (length prefix))))

(defun string-ends-with-p (string suffix)
  (and (>= (length string)
           (length suffix))
       (string= string suffix :start1 (- (length string) (length suffix)))))
