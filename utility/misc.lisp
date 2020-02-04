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

(defpackage :pfds.shcl.io/utility/misc
  (:use :common-lisp)
  (:export
   #:intern-conc #:cassert))
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
