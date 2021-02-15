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

(uiop:define-package :pfds.shcl.io/tests/common
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/tests/test-interface)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare-objects #:compare-reals #:compare)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:import-from :closer-mop)
  (:import-from :alexandria)
  (:import-from :prove
   #:subtest #:finalize)
  (:export
   #:token
   #:make-token
   #:token-p
   #:token-value
   #:^compare-equal-p
   #:shuffle
   #:encode
   #:decode
   #:build
   #:build-from-list
   #:named-subtests
   #:subtest*
   #:ensure-suite
   #:transform-interface
   #:wrap-interface
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
  (let ((result (compare-reals (token-value left) (token-value right))))
    (when (eq :equal result)
      (setf result :unequal))
    result))

(defun shuffle (list)
  (coerce (alexandria:shuffle (coerce list 'vector)) 'list))

(defun ^compare-equal-p (l r)
  (eq :equal (^compare l r)))

(defvar *problem-size* 1000)

(defparameter *sorted-numbers* (loop :for i :below *problem-size* :collect i))
(defparameter *even-numbers* (loop :for i :below *problem-size* :collect (* 2 i)))
(defparameter *odd-numbers* (mapcar '1+ *even-numbers*))
(defparameter *reverse-sorted-numbers* (reverse *sorted-numbers*))
(defparameter *random-numbers* (list* 666.0 666 666 (loop :for i :below *problem-size* :collect (random 1000))))
(defparameter *random-numbers-uniqued* (eql-unique *random-numbers*))
(defparameter *unequal-objects* (shuffle (loop :for i :below (/ *problem-size* 4) :nconc (loop :for j :below 4 :collect (make-token :value i)))))

(defun encode (item collection)
  (let ((type (^member-type collection)))
    (cond
      ((equal type t)
       item)
      ((equal type '(cons t t))
       (cons item (complex 0 (sxhash item))))
      (t
       (error "Not sure how to encode ~A" type)))))

(defun decode (item collection)
  (let ((type (^member-type collection)))
    (cond
      ((equal type t)
       item)
      ((equal type '(cons t t))
       (cassert (equal (complex 0 (sxhash (car item)))
                       (cdr item))
                (item)
                "Mismatched hash for ~W" item)
       (car item))
      (t
       (error "Not sure how to encode ~A" type)))))

(defun build-from-list (items)
  (let ((collection (^representative-empty)))
    (loop :for item :in items :do
      (setf collection (^with-member collection (encode item collection))))
    collection))

(defun build (&rest items)
  (build-from-list items))

(defmacro ensure-suite (&body body)
  (let ((old-suite (gensym "OLD-SUITE")))
    `(let* ((,old-suite prove:*suite*)
            (prove:*suite* (or ,old-suite
                               (make-instance 'prove:suite :plan nil))))
       (multiple-value-prog1
           (progn ,@body)
         (unless ,old-suite
           (finalize prove:*suite*))))))

(defconstant +test-name-prefix+ (if (boundp '+test-name-prefix+)
                                    (symbol-value '+test-name-prefix+)
                                    "TEST-"))

(defmacro subtest* (name &body body)
  (let ((name-val (gensym "NAME"))
        (done (gensym "DONE"))
        (again (gensym "AGAIN"))
        (stream (gensym "STREAM")))
    `(ensure-suite
       (let ((,name-val ,name))
         (block ,done
           (tagbody
              ,again
              (return-from ,done
                (subtest ,name-val
                  (restart-case
                      (progn ,@body)
                    (restart-test ()
                      :report (lambda (,stream) (format ,stream "Restart testing from ~A" ,name-val))
                      (go ,again)))))))))))

(defmacro named-subtests (&body body)
  (labels
      ((test-name (form)
         (and (consp form)
              (symbolp (car form))
              (nth-value 1 (alexandria:starts-with-subseq "TEST-" (symbol-name (car form)) :return-suffix t)))))
    `(ensure-suite
       ,@(loop :for form :in body
               :collect (let ((name (test-name form)))
                          (if name
                              `(subtest* ,name ,form)
                              form))))))

(defun transform-interface (interface &rest initargs &key &allow-other-keys)
  (let ((base-initargs
          (list*
           :debug-name (interface-instance-debug-name interface)
           (loop :for pair :in (interface-functions interface)
                 :for name = (car pair)
                 :nconc (list name (interface-get interface name))))))
    (apply 'make-instance (class-of interface) (concatenate 'list initargs base-initargs))))

(defun wrap-interface (interface &rest wrapper-specs &key &allow-other-keys)
  (let ((initargs
          (loop :with remaining-specs = wrapper-specs :while remaining-specs
                :for key = (pop remaining-specs) :for transformer = (pop remaining-specs)
                :nconc (list key (funcall transformer (interface-get interface key))))))

    (apply 'transform-interface interface initargs)))
