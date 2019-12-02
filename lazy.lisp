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

(defpackage :pfds.shcl.io/lazy
  (:use :common-lisp)
  (:import-from :bordeaux-threads #:make-lock #:with-lock-held)
  (:export #:lazy #:force))
(in-package :pfds.shcl.io/lazy)

(defmacro lazy (&body body)
  (let ((lock (gensym "LOCK"))
        (evaluated-p (gensym "EVALUATED-P"))
        (values (gensym "VALUES")))
    `(let ((,lock (make-lock "lazy block"))
           ,evaluated-p
           ,values)
       (lambda ()
         (with-lock-held (,lock)
           (unless ,evaluated-p
             (setf ,values (multiple-value-list (progn ,@body)))
             (setf ,evaluated-p t))
           (values-list ,values))))))

(defun force (suspension)
  (funcall suspension))

(define-compiler-macro force (&whole whole suspension)
  (if (and (consp suspension)
           (eq 'lazy (car suspension)))
      `(progn ,@(cdr suspension))
      whole))

(define-compiler-macro lazy (&whole whole &body body &environment env)
  (cond
    ((constantp `(progn ,@body) env)
     `(lambda () ,@body))

    ((and (null (cdr body))
          (consp (first body))
          (eq 'force (first (first body)))) 
     (return-from lazy (second (first body))))

    (t
     whole)))
