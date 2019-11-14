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

(defpackage :pfds.shcl.io/compare
  (:use :common-lisp)
  (:export
   #:compare #:define-type-id))
(in-package :pfds.shcl.io/compare)

(defgeneric compare (left right))

(defvar *type-id* 0)

(defgeneric type-id (object))

(defmethod type-id (object)
  nil)

(defmacro define-type-id (class-name)
  (let ((id (gensym "ID"))
        (object (gensym "OBJECT")))
    `(unless (find-method #'type-id nil '(,class-name) nil)
       (let ((,id (incf *type-id*)))
         (defmethod type-id ((,object ,class-name))
           ,id)))))

(define-type-id symbol)
(define-type-id package)
(define-type-id string)
(define-type-id number)
(define-type-id real)
(define-type-id vector)
(define-type-id array)
(define-type-id standard-object)
(define-type-id built-in-class)
(define-type-id character)
(define-type-id cons)

(defun compare-reals (left right)
  (cond
    ((> left right)
     :greater)
    ((< left right)
     :less)
    ((= left right)
     :equal)
    (t
     (assert nil (left right) "real numbers must be totally ordered!"))))

(defmethod compare (left right)
  (when (eql left right)
    (return-from compare :equal))
  (let* ((left-id (type-id left))
         (right-id (type-id right))
         (comparison (cond
                       ((and left-id right-id)
                        (compare-reals left-id right-id))
                       ;; Having an id now means that any type
                       ;; assigned an id later will have a larger one.
                       ;; So, if left has an id now but right doesn't
                       ;; then right's id can only be larger when its
                       ;; assigned.
                       (left-id
                        :less)
                       (right-id
                        :greater)
                       (t
                        :unequal))))
    (if (eq :equal comparison)
        :unequal
        (return-from compare comparison))))

(defmethod compare ((left real) (right real))
  (compare-reals left right))

(defmethod compare ((left character) (right character))
  (compare-reals (char-code left) (char-code right)))

(defmethod compare ((left vector) (right vector))
  (when (eql left right)
    (return-from compare :equal))

  (let ((comparison (compare-reals (length left) (length right))))
    (unless (eq :equal comparison)
      (return-from compare comparison)))

  (loop :for left-element :across left
        :for right-element :across right
        :for comparison = (compare left-element right-element) :do
          (unless (eq :equal comparison)
            (return-from compare comparison)))
  :equal)

(defmacro compare-many (first &body rest)
  (let ((compare-many (gensym "COMPARE-MANY"))
        (result (gensym "RESULT"))
        (value (gensym "VALUE")))
    `(block ,compare-many
       (let ((,result :equal))
         ,@(loop :for form :in (cons first rest) :collect
                 `(let ((,value ,form))
                    (ecase ,value
                      ((:less :greater)
                       (return-from ,compare-many ,value))
                      (:unequal
                       (setf ,result :uneqaul))
                      (:equal))))
         ,result))))

(defun chained-compare (compare-fn left right equal-requires-eql-p &rest getters)
  (when (eql left right)
    (return-from chained-compare :equal))

  (let ((result (if equal-requires-eql-p :uneqal :equal)))
    (dolist (getter getters)
      (let* ((left-property (funcall getter left))
             (right-property (funcall getter right))
             (comparison (funcall compare-fn left-property right-property)))
        (ecase comparison
          ((:less :greater)
           (return-from chained-compare comparison))
          (:unequal
           (setf result :unequal))
          (:equal))))

    result))

(defmethod compare ((left symbol) (right symbol))
  (chained-compare 'compare left right t 'symbol-name 'symbol-package))

(defmethod compare ((left package) (right package))
  (chained-compare 'compare left right t 'package-name))

(defmethod compare ((left cons) (right cons))
  (chained-compare 'compare left right nil 'car 'cdr))
