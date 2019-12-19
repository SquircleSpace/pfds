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

(defpackage :pfds.shcl.io/tests/compare
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/compare
   #:compare #:define-type-id #:compare-objects #:compare-objects-using-slots
   #:compare*)
  (:import-from :prove #:is #:subtest #:ok))
(in-package :pfds.shcl.io/tests/compare)

(defvar *constructors* (make-hash-table :test 'eq))

(defun register-constructor (class-name function)
  (setf (gethash class-name *constructors*) function))

(register-constructor 'null (lambda () (list nil)))

(defun symbol-constructor ()
  (list t (gensym "GENSYM") '#:symbol '#:symbol 'symbol 'symbol 'other-symbol))

(register-constructor 'symbol 'symbol-constructor)

(defun package-constructor ()
  (list (find-package :pfds.shcl.io/compare)
        (find-package :pfds.shcl.io/tests/compare)))

(register-constructor 'package 'package-constructor)

(defun string-constructor ()
  (labels
      ((random-string ()
         (with-output-to-string (stream)
           (loop :for i :below 2 :do
             (format stream "~A" (random (1+ i)))))))
    (list* "test" "string" "abc" (loop :for i :below 5 :collect (random-string)))))

(register-constructor 'string 'string-constructor)

(defun integer-constructor ()
  (list 0 1 2 3 -4 -10))

(register-constructor 'integer 'integer-constructor)

(defun ratio-constructor ()
  (list 7/22 20/7 -10/3))

(register-constructor 'ratio 'ratio-constructor)

(defun rational-constructor ()
  nil)

(register-constructor 'rational 'rational-constructor)

(defun float-constructor ()
  (list* 3.14 2.71 (loop :for i :below 3 :collect (random 1.0))))

(register-constructor 'float 'float-constructor)

(defun real-constructor ()
  nil)

(register-constructor 'real 'real-constructor)

(defun complex-constructor ()
  (list (complex 3.14159 2.718) (complex 7 22) (complex 3.14159 3)))

(register-constructor 'complex 'complex-constructor)

(defun number-constructor ()
  nil)

(register-constructor 'number 'number-constructor)

(defun vector-constructor ()
  (list* #(1 2 3) #(4) #()
         (loop :for i :below 2 :collect
           (let ((v (make-array 2)))
             (loop :for j :below 2 :do
               (setf (aref v j) (random 1.0)))
             v))))

(register-constructor 'vector 'vector-constructor)

(defun array-constructor ()
  (labels
      ((random-array (dimensions)
         (let ((array (make-array dimensions)))
           (loop :for index :below (array-total-size array)
                 :do (setf (row-major-aref array index) index))
           array)))
    (list* #2A((0 1) (2 3))
           #(1 2 3)
           (loop :for i :below 2 :nconc (loop :for j :below 2 :collect (random-array (list (1+ i) (1+ j))))))))

(register-constructor 'array 'array-constructor)

(defclass some-class ()
  ())

(defmethod compare-objects ((left some-class) (right some-class))
  (compare-objects-using-slots left right))

(defclass another-class (some-class)
  ((value
    :initform 123)))

(defmethod compare-objects ((left another-class) (right another-class))
  (compare-objects-using-slots left right 'value))

(defclass final-class () ())

(defun class-constructor ()
  (list (find-class 'standard-class)
        (find-class 'standard-object)
        (find-class 'some-class)
        (find-class 'another-class)
        (find-class 'final-class)
        (make-instance 'standard-class)))

(register-constructor 'standard-class 'class-constructor)

(defun object-constructor ()
  (list (make-instance 'some-class)
        (make-instance 'another-class)
        (make-instance 'another-class)
        (make-instance 'final-class)
        (make-instance 'standard-object)
        (make-instance 'standard-object)))

(register-constructor 'standard-object 'object-constructor)

(defun built-in-class-constructor ()
  (list (find-class 'cons)
        (find-class 'integer)))

(register-constructor 'built-in-class 'built-in-class-constructor)

(defun character-constructor ()
  (list #\a #\b #\c))

(register-constructor 'character 'character-constructor)

(defun cons-constructor ()
  (list (cons 1 2) (list* 1 2 3 4) (list 1 2 3 4) (list 3 4 5)))

(register-constructor 'cons 'cons-constructor)

(defun pathname-constructor ()
  (list #P"/foo/bar.txt" #P"../baz/bap.fiz" #P"../baz/bap" #P"../baz/bap/"))

(register-constructor 'pathname 'pathname-constructor)

(defun function-constructor ()
  (list #'function-constructor (lambda () 123) (constantly t)))

(register-constructor 'function 'function-constructor)

(defun compare-< (comparator left right)
  (eq :less (funcall comparator left right)))

(defun sorted (comparator sequence)
  (sort (copy-seq sequence) (lambda (l r) (compare-< comparator l r))))

(defun split-equals (comparator list comparison)
  (let* ((first (first list))
         (rest (cdr list))
         (equals (list first)))
    (loop :while rest
          :while (or (eq comparison (funcall comparator first (car rest)))
                     (eql first (car rest)))
          :do (push (pop rest) equals))
    (values equals rest)))

(defun validate-equals (comparator equals rest comparison)
  (loop :for first-tail :on equals
        :for first = (car first-tail)
        :do (loop :for second :in (cdr first-tail) :do
                  (assert (or (eql first second)
                              (eq comparison (funcall comparator first second))) nil
                          "equal or unequal things should all be mutually equal/unequal")))
  (loop :for equal :in equals
        :do (loop :for other :in rest :do
          (assert (eq :less (funcall comparator equal other)) nil
                  "equal or unequal things should all behave the same when compared to other objects"))))

(defun validate-ordering (comparator list)
  (unless (and list (cdr list))
    (return-from validate-ordering))

  (loop :for list-head :on list :do
    (when (cdr list-head)
      (let* ((first (first list-head))
             (first-comparison (funcall comparator first (second list-head)))
             (tail (ecase first-comparison
                     (:less
                      (cdr (cdr list-head)))
                     ((:equal :unequal)
                      (multiple-value-bind (equals rest) (split-equals comparator list-head first-comparison)
                        (validate-equals comparator equals rest first-comparison)
                        rest))
                     (:greater
                      (error "~A and ~A should compare as less but they comapre as greater"
                             first (second list-head))))))
        (loop :for second :in tail
              :for comparison = (funcall comparator first second)
              :do
          (ecase comparison
            (:less)
            ((:equal :unequal :greater)
             (error "Unexpected comparison result ~A for ~A ~A"
                    comparison first second))))))))

(defun make-test-objects ()
  (labels
      ((get-objects (type fn)
         (let ((result (funcall fn)))
           (loop :for object :in result :do
             (unless (typep object type)
               (error "Constructor ~A returned object not of type ~A: ~A" fn type object)))
           result)))
    (loop :for type :being :the :hash-keys :of *constructors* :using (hash-value fn) :nconc (get-objects type fn))))

(defun test-ordering (comparator)
  (let ((objects (make-test-objects)))
    (setf objects (sorted comparator objects))
    (validate-ordering comparator objects))
  t)

(defun run-tests (&optional (comparator 'compare))
  (ok (test-ordering comparator)
      "compare seems to produce well ordered results"))
