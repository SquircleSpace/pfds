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
   #:comparison
   #:compare
   #:compare-objects
   #:compare-objects-using-slots
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
   #:unequalify
   #:define-type-id))
(in-package :pfds.shcl.io/compare)

(deftype comparison ()
  '(member :greater :less :equal :unequal))

(defgeneric compare-objects (left right)
  (:documentation
   "Establish ordering between two objects.

Any methods you define on this generic function must obey the laws
described in `COMPARE''s function documentation.  This generic
function is not intended to be called directly.  You should call
`COMPARE' instead."))

(defun compare-objects-using-slots (left right &rest slots)
  (when (eql left right)
    (return-from compare-objects-using-slots :equal))

  (let ((result :equal))
    (dolist (slot slots)
      (let ((slot-result (etypecase slot
                           (function
                            (compare (funcall slot left) (funcall slot right)))
                           (symbol
                            (compare (slot-value left slot) (slot-value right slot))))))
        (ecase slot-result
          (:equal)
          (:unequal
           (setf result :unequal))
          ((:greater :less)
           (return-from compare-objects-using-slots slot-result)))))
    result))

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

(define-type-id symbol)
(define-type-id package)
(define-type-id integer)
(define-type-id ratio)
(define-type-id rational)
(define-type-id float)
(define-type-id real)
(define-type-id complex)
(define-type-id number)
(define-type-id vector)
(define-type-id string)
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

(declaim (inline unequalify))
(defun unequalify (value)
  (if (eq value :equal)
      :unequal
      value))

(declaim (inline compare-reals))
(defun compare-reals (left right)
  (declare (type real left right))
  (cond
    ((> left right)
     :greater)
    ((< left right)
     :less)
    ((eql left right)
     :equal)
    (t
     (locally
         ;; Just to quiet the compiler.  Otherwise it will get all
         ;; uppity when we declaim compare-classes as inline, but
         ;; we're only doing that so we can make it locally inline in
         ;; the method definition at the bottom!
         (declare (notinline compare-classes))
       (unequalify (compare-classes (class-of left) (class-of right)))))))

(declaim (inline compare-characters))
(defun compare-characters (left right)
  (declare (type character left right))
  (compare-reals (char-code left) (char-code right)))

(declaim (inline compare-vectors))
(defun compare-vectors (left right &optional (element-compare-fn 'compare))
  (declare (type vector left right)
           (type (or symbol function) element-compare-fn))
  (when (eql left right)
    (return-from compare-vectors :equal))

  (when (symbolp element-compare-fn)
    (setf element-compare-fn (symbol-function element-compare-fn)))

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
(declaim (notinline compare-vectors))

(declaim (inline compare-strings))
(defun compare-strings (left right)
  (declare (type string left right))
  (compare-vectors left right #'compare-characters))

(declaim (inline compare-packages))
(defun compare-packages (left right)
  (declare (type package left right))
  (when (eql left right)
    (return-from compare-packages :equal))

  (unequalify
   (compare-strings (package-name left) (package-name right))))
(declaim (notinline compare-packages))

(declaim (inline compare-packages-or-nil))
(defun compare-packages-or-nil (left right)
  (declare (type (or package null) left right))
  (cond
    ((and (null left) (null right))
     :equal)
    ((null left)
     :less)
    ((null right)
     :greater)
    (t
     (compare-packages left right))))
(declaim (notinline compare-packages-or-nil))

(declaim (inline compare-symbols))
(defun compare-symbols (left right)
  (declare (type symbol left right))
  (when (eql left right)
    (return-from compare-symbols :equal))

  (unequalify
   (compare*
     (compare-strings (symbol-name left) (symbol-name right))
     (compare-packages-or-nil (symbol-package left) (symbol-package right)))))
(declaim (notinline compare-symbols))

(declaim (inline compare-classes))
(defun compare-classes (left right)
  (declare (type class left right))
  (when (eql left right)
    (return-from compare-classes :equal))

  (unequalify
   (compare-symbols (class-name left) (class-name right))))
(declaim (notinline compare-classes))

(declaim (inline compare-complexes))
(defun compare-complexes (left right)
  (declare (type complex left right))
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
(declaim (notinline compare-complexes))

(declaim (inline compare-type-ids))
(defun compare-type-ids (left right)
  (declare (type (or integer null) left right))
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

(declaim (inline compare-arrays))
(defun compare-arrays (left right &optional (element-compare-fn 'compare))
  (declare (type array left right))
  (when (eql left right)
    (return-from compare-arrays :equal))

  (when (symbolp element-compare-fn)
    (setf element-compare-fn (symbol-function element-compare-fn)))

  (let ((comparison (compare-lists (array-dimensions left)
                                   (array-dimensions right)
                                   #'compare-reals)))
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

    result))
(declaim (notinline compare-arrays))

(declaim (inline canonical-class-of))
(defun canonical-class-of (object)
  (cond
    ((stringp object)
     #.(find-class 'string))
    ((arrayp object)
     #.(find-class 'array))
    ((realp object)
     #.(find-class 'real))
    (t
     (class-of object))))

(declaim (inline compare-cons))
(defun compare-cons (left right &optional (car-compare-fn 'compare) (cdr-compare-fn 'compare))
  (declare (type cons left right))
  (when (eql left right)
    (return-from compare-cons :equal))

  (compare*
   (funcall car-compare-fn (car left) (car right))
   (funcall cdr-compare-fn (cdr left) (cdr right))))
(declaim (notinline compare-cons))

(defun compare-lists (left right &optional (car-compare-fn 'compare))
  (declare (type list left right))
  (when (eql left right)
    (return-from compare-lists :equal))
  (unless left
    (return-from compare-lists :less))
  (unless right
    (return-from compare-lists :greater))

  (compare*
    (funcall car-compare-fn (car left) (car right))
    (compare-lists (cdr left) (cdr right) car-compare-fn)))

(declaim (inline compare-pathnames))
(defun compare-pathnames (left right)
  (declare (type pathname left right))
  (when (eql left right)
    (return-from compare-pathnames :equal))

  (compare*
   (compare (pathname-name left) (pathname-name right))
   (compare (pathname-type left) (pathname-type right))
   (compare (pathname-directory left) (pathname-directory right))
   (compare (pathname-device left) (pathname-device right))
   (compare (pathname-host left) (pathname-host right))
   (compare (pathname-version left) (pathname-version right))))
(declaim (notinline compare-pathnames))

(declaim (inline compare-functions))
(defun compare-functions (left right
                          &optional (lambda-expression-compare-fn 'compare)
                            (closure-p-compare-fn 'compare)
                            (name-compare-fn 'compare))
  (declare (type function left right))
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
         (funcall lambda-expression-compare-fn left-expression right-expression))))))
(declaim (notinline compare-functions))

(declaim (inline compare-other-objects))
(defun compare-other-objects (left right)
  (if (eq left right)
      :equal
      :unequal))

(defun compare (left right)
  "Determine the relative ordering between `LEFT' and `RIGHT'.

This function will return either `:GREATER', `:LESS', `:EQUAL', or
`:UNEQUAL'.  This function is a wrapper around `COMPARE-OBJECTS'.  If
`LEFT' and `RIGHT' are eql then `COMPARE' will return `:EQUAL' without
calling `COMPARE-OBJECTS'.

The ordering provided by this function obeys the transitivity law.
That means that if X and Y have ordering C and Y and Z also have
ordering C, then X and Z have ordering C as well.  In code, this means
the following assertion should never fail
    (let ((a (compare x y))
          (b (compare y z)))
      (when (eq a b)
        (assert (eq a (compare x z)))))

The ordering provided by this function also obeys what we'll call the
\"sustainability\" property for equality and unequality.  Suppose X
and Y are `:EQUAL' or `:UNEQUAL'.  For any object Z, X and Z will
always have the same result as Y and Z.  In code, this means the
following assertion should never fail.
    (lambda (x y z) ; for any objects x, y, and z
      (let ((a (compare x y)))
        (when (typep a '(member :EQUAL :UNEQUAL))
          (assert (eq (compare x z) (compare y z))))))"
  (when (eql left right)
    (return-from compare :equal))

  (let ((left-class (canonical-class-of left))
        (right-class (canonical-class-of right)))
    (if (eq left-class right-class)
        (compare-objects left right)
        (unequalify (compare-classes left-class right-class)))))

(defmethod compare-objects (left right)
  (locally
      (declare (inline compare-other-objects))
    (compare-other-objects left right)))

(defmethod compare-objects ((left class) (right class))
  (locally
      (declare (inline compare-classes))
    (compare-classes left right)))

(defmethod compare-objects ((left real) (right real))
  (locally
      (declare (inline compare-reals))
    (compare-reals left right)))

(defmethod compare-objects ((left complex) (right complex))
  (locally
      (declare (inline compare-complexes))
    (compare-complexes left right)))

(defmethod compare-objects ((left character) (right character))
  (locally
      (declare (inline compare-characters))
    (compare-characters left right)))

(defmethod compare-objects ((left vector) (right vector))
  (locally
      (declare (inline compare-vectors))
    (compare-vectors left right)))

(defmethod compare-objects ((left array) (right array))
  (locally
      (declare (inline compare-arrays))
    (compare-arrays left right)))

(defmethod compare-objects ((left string) (right string))
  (locally
      (declare (inline compare-strings))
    (compare-strings left right)))

(defmethod compare-objects ((left symbol) (right symbol))
  (locally
      (declare (inline compare-symbols))
    (compare-symbols left right)))

(defmethod compare-objects ((left package) (right package))
  (locally
      (declare (inline compare-packages))
    (compare-packages left right)))

(defmethod compare-objects ((left cons) (right cons))
  (locally
      (declare (inline compare-cons))
    (compare-cons left right #'compare #'compare)))

(defmethod compare-objects ((left pathname) (right pathname))
  (locally
      (declare (inline compare-pathnames))
    (compare-pathnames left right)))

(defmethod compare-objects ((left function) (right function))
  (locally
      (declare (inline compare-functions))
    (compare-functions left right)))
