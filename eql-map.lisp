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

(defpackage :pfds.shcl.io/eql-map
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common #:define-immutable-structure #:to-list)
  (:export
   #:make-eql-map
   #:make-eql-map*
   #:eql-map-p
   #:eql-map
   #:eql-map-with
   #:eql-map-without
   #:eql-map-lookup
   #:eql-map-count
   #:eql-map-union
   #:eql-map-intersection
   #:eql-map-difference
   #:eql-map-representative
   #:do-eql-map))
(in-package :pfds.shcl.io/eql-map)

(define-immutable-structure (eql-map (:constructor %make-eql-map) (:copier %copy-eql-map))
  (table (make-hash-table :test 'eql) :type hash-table))

(defmethod to-list ((eql-map eql-map))
  (loop :for key :being :the :hash-keys :of (eql-map-table eql-map)
          :using (hash-value value) :collect (cons key value)))

(defmethod print-object ((eql-map eql-map) stream)
  (write
   `(make-eql-map* :alist (quote ,(to-list eql-map)))
   :stream stream))

(defun make-eql-map* (&key plist alist)
  (let ((new-table (make-hash-table :test 'eql)))
    (loop :while plist
          :for key = (pop plist)
          :for value = (if plist (pop plist) (error "Odd number of items in plist"))
          :do (setf (gethash key new-table) value))
    (dolist (pair alist)
      (setf (gethash (car pair) new-table) (cdr pair)))
    (%make-eql-map :table new-table)))

(defun make-eql-map (&rest plist)
  (make-eql-map* :plist plist))

(defun copy-hash-table (hash-table)
  (let ((new-table (make-hash-table :test (hash-table-test hash-table)
                                    :size (hash-table-size hash-table)
                                    :rehash-size (hash-table-rehash-size hash-table)
                                    :rehash-threshold (hash-table-rehash-threshold hash-table))))
    (loop :for key :being :the :hash-keys :of hash-table
          :using (:hash-value value) :do
            (setf (gethash key new-table) value))
    new-table))

(defun copy-eql-map (eql-map)
  (%make-eql-map :table (copy-hash-table (eql-map-table eql-map))))

(defun eql-map-with (eql-map key &optional value)
  (multiple-value-bind (old-value found-p) (gethash key (eql-map-table eql-map))
    (when (and found-p (eql old-value value))
      (return-from eql-map-with eql-map)))

  (let ((new-map (copy-eql-map eql-map)))
    (setf (gethash key (eql-map-table new-map)) value)
    new-map))

(defun eql-map-without (eql-map key)
  (unless (nth-value 1 (gethash key (eql-map-table eql-map)))
    (return-from eql-map-without eql-map))

  (let ((new-map (copy-eql-map eql-map)))
    (remhash key (eql-map-table new-map))
    new-map))

(defun eql-map-lookup (eql-map key)
  (gethash key (eql-map-table eql-map)))

(defun eql-map-count (eql-map)
  (hash-table-count (eql-map-table eql-map)))

(defun choose-first (first second)
  (declare (ignore second))
  first)

(defun eql-map-union (first second &optional (resolver 'choose-first))
  (when (eql first second)
    (return-from eql-map-union first))

  (let (swapped-p)
    (unless (>= (hash-table-count (eql-map-table first))
                (hash-table-count (eql-map-table second)))
      (rotatef first second)
      (setf swapped-p t))

    (labels
        ((resolve (first-value second-value)
           (when swapped-p
             (rotatef first-value second-value))
           (funcall resolver first-value second-value)))
      (declare (inline resolve))

      (let ((new-map (copy-eql-map first)))
        (loop :for key :being :the :hash-keys :of (eql-map-table second)
                :using (hash-value second-value) :do
                  (multiple-value-bind (first-value found-p) (gethash key (eql-map-table new-map))
                    (unless (and found-p (eql first-value second-value))
                      (setf (gethash key (eql-map-table new-map))
                            (if found-p
                                (resolve first-value second-value)
                                second-value)))))
        new-map))))

(defun eql-map-intersection (first second &optional (resolver 'choose-first))
  (when (eql first second)
    (return-from eql-map-intersection first))

  (let (swapped-p)
    (unless (>= (hash-table-count (eql-map-table first))
                (hash-table-count (eql-map-table second)))
      (rotatef first second)
      (setf swapped-p t))

    (labels
        ((resolve (first-value second-value)
           (when swapped-p
             (rotatef first-value second-value))
           (funcall resolver first-value second-value)))
      (declare (inline resolve))

      (let ((new-map (make-eql-map)))
        (loop :for key :being :the :hash-keys :of (eql-map-table second)
                :using (hash-value second-value) :do
                  (multiple-value-bind (first-value found-p) (gethash key (eql-map-table first))
                    (when found-p
                      (setf (gethash key (eql-map-table new-map))
                            (if (not (eql first-value second-value))
                                (resolve first-value second-value)
                                second-value)))))
        new-map))))

(defun eql-map-difference (first second)
  (when (eql first second)
    (return-from eql-map-difference (make-eql-map)))

  (let ((new-map (make-eql-map)))
    (loop :for key :being :the :hash-keys :of (eql-map-table first)
          :using (hash-value value) :do
            (unless (nth-value 1 (gethash key (eql-map-table second)))
              (setf (gethash key (eql-map-table new-map)) value)))
    new-map))

(defun eql-map-representative (eql-map)
  (loop :for key :being :the :hash-keys :of (eql-map-table eql-map)
        :using (hash-value value) :do
    (return-from eql-map-representative (values key value t)))
  (values nil nil nil))

(defmacro do-eql-map ((key value eql-map &optional result) &body body)
  (let ((our-key (gensym "KEY"))
        (our-value (gensym "VALUE")))
    `(loop :for ,our-key :being :the :hash-keys :of (eql-map-table ,eql-map)
           :using (hash-value ,our-value) :do
             (let ((,key ,our-key)
                   (,value ,our-value))
               ,@body)
           :finally (return ,result))))
