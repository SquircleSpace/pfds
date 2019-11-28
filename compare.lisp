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
   #:compare
   #:compare-reals
   #:compare-complexes
   #:compare-characters
   #:compare-vectors
   #:compare-arrays
   #:compare-strings
   #:compare-packages
   #:compare-symbols
   #:compare-classes
   #:compare-cons
   #:compare-lists
   #:compare-pathnames
   #:compare-functions
   #:compare-other-objects
   #:compare*
   #:define-type-id))
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
       (let ((,id *type-id*))
         (incf *type-id*)
         (defmethod type-id ((,object ,class-name))
           ,id)))))

(define-type-id null)
(define-type-id symbol)
(define-type-id package)
(define-type-id string)
(define-type-id integer)
(define-type-id ratio)
(define-type-id rational)
(define-type-id float)
(define-type-id real)
(define-type-id complex)
(define-type-id number)
(define-type-id vector)
(define-type-id array)
(define-type-id standard-class)
(define-type-id standard-object)
(define-type-id built-in-class)
(define-type-id character)
(define-type-id cons)
(define-type-id pathname)
(define-type-id function)

(defmacro compare* (&body forms)
  (let ((result (gensym "RESULT"))
        (value (gensym "VALUE"))
        (compare* (gensym "COMPARE*")))
    `(block ,compare*
       (let ((,result :equal))
         ,@(loop :for form :in forms
                 :collect
                 `(let ((,value ,form))
                    (ecase ,value
                      (:equal)
                      (:unequal
                       (setf ,result :unequal))
                      ((:less :greater)
                       (return-from ,compare* ,value)))))
         ,result))))

(defun unequalify (value)
  (if (eq value :equal)
      :unequal
      value))

(defun compare-reals (left right)
  (cond
    ((> left right)
     :greater)
    ((< left right)
     :less)
    ((eql left right)
     :equal)
    (t
     (unequalify (compare-type-ids (type-id left) (type-id right))))))

(defun compare-complexes (left right)
  ;; There is no way to order complex numbers... if you care about
  ;; algebra working consistently.  This ordering relation has
  ;; transitivity, and that's good enough for use in sets and maps.
  ;; Sure, the order they appear in might not make sense for
  ;; mathematical contexts, but its better than not providing a
  ;; comparison function!  Not providing a comparison function just
  ;; means that complex numbers appear in an unpredictable order in
  ;; sets and maps... and we pay a significant performance penalty to
  ;; boot.
  (when (eql left right)
    (return-from compare-complexes :equal))

  (compare*
   (compare-reals (realpart left) (realpart right))
   (compare-reals (imagpart left) (imagpart right))))

(defun compare-type-ids (left right)
  (cond
    ((and left right)
     (cond
       ((> left right)
        :greater)
       ((< left right)
        :less)
       ((= left right)
        :equal)))
    ;; Having an id now means that any type
    ;; assigned an id later will have a larger one.
    ;; So, if left has an id now but right doesn't
    ;; then right's id can only be larger when its
    ;; assigned.
    (left
     :less)
    (right
     :greater)
    (t
     :unequal)))

(defun compare-characters (left right)
  (compare-reals (char-code left) (char-code right)))

(defun compare-vectors (left right &key (element-compare-fn 'compare))
  (when (eql left right)
    (return-from compare-vectors :equal))

  (let ((comparison (compare-reals (length left) (length right))))
    (unless (eq :equal comparison)
      (return-from compare-vectors comparison)))

  (let ((result :equal))
    (loop :for left-element :across left
          :for right-element :across right :do
            (let ((comparison (funcall element-compare-fn left-element right-element)))
              (ecase comparison
                (:equal)
                (:unequal
                 (setf result :unequal))
                ((:greater :less)
                 (return-from compare-vectors comparison)))))

    result))

(defun compare-arrays (left right &key (element-compare-fn 'compare))
  (when (eql left right)
    (return-from compare-arrays :equal))

  (labels
      ((list-comparator (left right)
         (compare-lists left right
                        :car-compare-fn 'compare-reals
                        :cdr-compare-fn #'list-comparator)))
    (let ((comparison (list-comparator (array-dimensions left)
                                       (array-dimensions right))))
      (unless (eq :equal comparison)
        (return-from compare-arrays comparison)))

    (let ((result :equal))
      (loop :for index :below (array-total-size left) :do
        (let ((comparison (funcall element-compare-fn
                                   (row-major-aref left index)
                                   (row-major-aref right index))))
          (ecase comparison
            (:equal)
            (:unequal
             (setf result :unequal))
            ((:greater :less)
             (return-from compare-arrays comparison)))))

      result)))

(defun compare-strings (left right)
  (compare-vectors left right :element-compare-fn 'compare-characters))

(defun compare-packages (left right)
  (when (eql left right)
    (return-from compare-packages :equal))

  (unequalify
   (compare*
    (compare-strings (package-name left) (package-name right))
    ;; Packages should never have the same name and be un-eql.  So,
    ;; the above comparison should always establish an ordering.  Just
    ;; in case that logic fails for some reason, let's give it one
    ;; last shot to establish an ordering!
    (compare-reals (sxhash left) (sxhash right)))))

(defun compare-symbols (left right)
  (when (eql left right)
    (return-from compare-symbols :equal))

  (unequalify
   (compare*
    (compare-strings (symbol-name left) (symbol-name right))
    (compare-packages (symbol-package left) (symbol-package right)))))

(defun compare-classes (left right)
  (when (eql left right)
    (return-from compare-classes :equal))

  (unequalify
   (compare*
    (compare-symbols (class-name left) (class-name right))
    (compare-reals (sxhash left) (sxhash right)))))

(defun compare-cons (left right &key (car-compare-fn 'compare) (cdr-compare-fn 'compare))
  (when (eql left right)
    (return-from compare-cons :equal))

  (compare*
   (funcall car-compare-fn (car left) (car right))
   (funcall cdr-compare-fn (cdr left) (cdr right))))

(defun compare-lists (left right &key (car-compare-fn 'compare) (cdr-compare-fn 'compare))
  (when (eql left right)
    (return-from compare-lists :equal))
  (unless left
    (return-from compare-lists :less))
  (unless right
    (return-from compare-lists :greater))

  (compare*
    (funcall car-compare-fn (car left) (car right))
    (funcall cdr-compare-fn (cdr left) (cdr right))))

(defun compare-pathnames (left right)
  (when (eql left right)
    (return-from compare-pathnames :equal))

  (compare*
   (compare (pathname-name left) (pathname-name right))
   (compare (pathname-type left) (pathname-type right))
   (compare (pathname-directory left) (pathname-directory right))
   (compare (pathname-device left) (pathname-device right))
   (compare (pathname-host left) (pathname-host right))
   (compare (pathname-version left) (pathname-version right))))

(defun compare-functions (left right
                          &key (lambda-expression-compare-fn 'compare)
                            (closure-p-compare-fn 'compare)
                            (name-compare-fn 'compare))
  (when (eql left right)
    (return-from compare-functions :equal))

  (multiple-value-bind
        (left-expression left-closure-p left-name) (function-lambda-expression left)
    (multiple-value-bind
          (right-expression right-closure-p right-name) (function-lambda-expression right)
      (unequalify
       (compare*
         ;; name is most likely to be provided, meaningful, and
         ;; easily comparable.  Let's start with that.
         (funcall name-compare-fn left-name right-name)
         (funcall closure-p-compare-fn left-closure-p right-closure-p)
         (funcall lambda-expression-compare-fn left-expression right-expression)
         ;; Both CCL and SBCL always return the same value for the
         ;; hash of a function.  But, let's give it a shot anyway.
         (compare-reals (sxhash left) (sxhash right)))))))

(defun compare-other-objects (left right)
  (when (eql left right)
    (return-from compare-other-objects :equal))

  (unequalify
   (compare*
    ;; Are these known types?
    (compare-reals (type-id left) (type-id right))
    ;; Can we establish an ordering anyway?
    (compare-classes (class-of left) (class-of right))
    ;; When in doubt, hash it out!
    (compare-reals (sxhash left) (sxhash right)))))

(defmethod compare (left right)
  (compare-other-objects left right))

(defmethod compare ((left real) (right real))
  (compare-reals left right))

(defmethod compare ((left complex) (right complex))
  (compare-complexes left right))

(defmethod compare ((left character) (right character))
  (compare-characters left right))

(defmethod compare ((left vector) (right vector))
  (compare-vectors left right))

(defmethod compare ((left array) (right array))
  (compare-arrays left right))

(defmethod compare ((left string) (right string))
  (compare-strings left right))

(defmethod compare ((left symbol) (right symbol))
  (compare-symbols left right))

(defmethod compare ((left package) (right package))
  (compare-packages left right))

(defmethod compare ((left list) (right list))
  (compare-lists left right))

(defmethod compare ((left pathname) (right pathname))
  (compare-pathnames left right))

(defmethod compare ((left function) (right function))
  (compare-functions left right))
