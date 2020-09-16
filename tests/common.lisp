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

(defpackage :pfds.shcl.io/tests/common
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare #:compare-objects)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:import-from :closer-mop)
  (:import-from :alexandria)
  (:import-from :prove
   #:is #:subtest)
  (:export
   #:check-common-consistency
   #:check-interface-conformance
   #:do-type-records
   #:token
   #:make-token
   #:token-p
   #:token-value
   #:compare-equal-p
   #:compare-less-p
   #:shuffle
   #:cons-to-item
   #:item-to-cons
   #:encode
   #:decode
   #:build
   #:build-from-list
   #:make
   #:make-test-environment
   #:with-test-environment
   #:named-subtests
   #:subtest*
   #:*name*
   #:*comparator*
   #:*maker*
   #:*problem-size*
   #:*sorted-numbers*
   #:*even-numbers*
   #:*odd-numbers*
   #:*reverse-sorted-numbers*
   #:*random-numbers*
   #:*random-numbers-uniqued*
   #:*unequal-objects*))
(in-package :pfds.shcl.io/tests/common)

(defun eql-unique (items)
  (let ((table (make-hash-table :test 'eql))
        collected)
    (loop :for item :in items :do
      (unless (gethash item table)
        (setf (gethash item table) t)
        (push item collected)))
    (nreverse collected)))

(define-immutable-structure token
  value)

(defmethod compare-objects ((left token) (right token))
  (when (eql left right)
    (return-from compare-objects :equal))
  (let ((result (compare (token-value left) (token-value right))))
    (when (eq :equal result)
      (setf result :unequal))
    result))

(defun item-to-cons (item)
  (cons item (complex 0 (sxhash item))))

(defun cons-to-item (cons)
  (cassert (equal (cdr cons) (complex 0 (sxhash (car cons))))
           (cons) "Cons must not be corrupted")
  (car cons))

(defun shuffle (list)
  (coerce (alexandria:shuffle (coerce list 'vector)) 'list))

(defun compare-equal-p (l r)
  (eq :equal (compare l r)))

(defun compare-less-p (l r)
  (eq :less (compare l r)))

(defvar *problem-size* 1000)

(defparameter *sorted-numbers* (loop :for i :below *problem-size* :collect i))
(defparameter *even-numbers* (loop :for i :below *problem-size* :collect (* 2 i)))
(defparameter *odd-numbers* (mapcar '1+ *even-numbers*))
(defparameter *reverse-sorted-numbers* (reverse *sorted-numbers*))
(defparameter *random-numbers* (list* 666.0 666 666 (loop :for i :below *problem-size* :collect (random 1000))))
(defparameter *random-numbers-uniqued* (eql-unique *random-numbers*))
(defparameter *unequal-objects* (shuffle (loop :for i :below (/ *problem-size* 4) :nconc (loop :for j :below 4 :collect (make-token :value i)))))

(defvar *encode*)
(defvar *decode*)
(defvar *maker*)
(defvar *maker-initargs* nil)
(defvar *maker-positional-args*)
(defvar *maker-items-initarg*)
(defvar *name*)
(defvar *comparator*)

(defun encode (item)
  (funcall *encode* item))

(defun decode (item)
  (funcall *decode* item))

(defun make (&rest args &key &allow-other-keys)
  (apply *maker* (concatenate 'list *maker-positional-args* *maker-initargs* args)))

(defun build-from-list (items)
  (make *maker-items-initarg* (mapcar #'encode items)))

(defun build (&rest items)
  (build-from-list items))

(defun make-list* (&key items)
  items)

(defun find-maker (class-name)
  (if (eq class-name 'list)
      'make-list*
      (multiple-value-bind (sym state) (find-symbol (format nil "MAKE-~A" class-name) (symbol-package class-name))
        (when (eq :external state)
          sym))))

(defun other-compare (l r)
  (compare l r))

(defun make-test-environment (class-name &rest initargs)
  (let* ((interfaces (interfaces-implemented class-name))
         (map-p (find 'ordered-map interfaces))
         (ordered-p (find 'ordered-collection interfaces)))
    `((*encode* . ,(if map-p 'item-to-cons 'identity))
      (*decode* . ,(if map-p 'cons-to-item 'identity))
      (*maker* . ,(find-maker class-name))
      (*maker-items-initarg* . ,(if map-p :alist :items))
      (*maker-positional-args* . ,(when ordered-p '(other-compare)))
      (*comparator* . ,(when ordered-p 'other-compare))
      (*maker-initargs* . ,initargs)
      (*name* . ,class-name))))

(defmacro with-test-environment (environment &body body)
  (let ((record-val (gensym "RECORD")))
    `(let ((,record-val ,environment))
       (progv (mapcar #'car ,record-val) (mapcar #'cdr ,record-val)
         ,@body))))

(defconstant +test-name-prefix+ (if (boundp '+test-name-prefix+)
                                    (symbol-value '+test-name-prefix+)
                                    "TEST-"))

(defmacro subtest* (name &body body)
  (let ((name-val (gensym "NAME"))
        (done (gensym "DONE"))
        (again (gensym "AGAIN"))
        (stream (gensym "STREAM")))
    `(let ((,name-val ,name))
       (block ,done
         (tagbody
            ,again
            (return-from ,done
              (subtest ,name-val
                (restart-case
                    (progn ,@body)
                  (restart-test ()
                    :report (lambda (,stream) (format ,stream "Restart testing from ~A" ,name-val))
                    (go ,again))))))))))

(defmacro named-subtests (&body body)
  (labels
      ((test-name (form)
         (and (consp form)
              (symbolp (car form))
              (nth-value 1 (alexandria:starts-with-subseq "TEST-" (symbol-name (car form)) :return-suffix t)))))
    `(progn ,@(loop :for form :in body
                    :collect (let ((name (test-name form)))
                               (if name
                                   `(subtest* ,name ,form)
                                   form))))))
