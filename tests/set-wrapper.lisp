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

(defpackage :pfds.shcl.io/tests/set-wrapper
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:import-from :pfds.shcl.io/utility/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare #:compare-objects)
  (:import-from :pfds.shcl.io/utility/misc #:cassert)
  (:export
   #:set-wrapper
   #:set-wrapper-p
   #:make-set-wrapper
   #:unwrap-map
   #:wrap-map))
(in-package :pfds.shcl.io/tests/set-wrapper)

(define-immutable-structure (set-wrapper (:constructor %make-set-wrapper))
  (map (error "required")))

(declare-interface-conformance set-wrapper ordered-set)

(defun make-set-wrapper (comparator &key items (maker (error "maker is required")))
  (%make-set-wrapper :map (funcall maker comparator :alist (mapcar (lambda (i) (cons i (value-for-key i))) items))))

(defun wrap-map (map)
  (%make-set-wrapper :map map))

(defun unwrap-map (set-wrapper)
  (set-wrapper-map set-wrapper))

(defun rewrap-results (original-set results)
  (destructuring-bind (new &rest rest) results
    (values-list
     (if (eql new (unwrap-map original-set))
         (cons original-set rest)
         (cons (wrap-map new) rest)))))

(defmethod is-empty ((set set-wrapper))
  (is-empty (unwrap-map set)))

(defun check-wrapped-key-value-pair (key value)
  (let ((expected (sxhash key)))
    (cassert (equal value expected) nil
             "Value mismatch.  Got ~W, expected ~W" value expected)
    value))

(defun value-for-key (key)
  (sxhash key))

(defmethod empty ((set set-wrapper))
  (rewrap-results set (multiple-value-list (empty (unwrap-map set)))))

(defmethod with-member ((set set-wrapper) key)
  (rewrap-results set (multiple-value-list (with-entry (unwrap-map set) key (value-for-key key)))))

(defmethod without-member ((set set-wrapper) key)
  (rewrap-results set (multiple-value-list (without-entry (unwrap-map set) key))))

(defmethod is-member ((set set-wrapper) key)
  (nth-value 1 (lookup-entry (unwrap-map set) key)))

(defmethod to-list ((set set-wrapper))
  (mapcar 'car (to-list (unwrap-map set))))

(defmethod for-each ((set set-wrapper) function)
  (for-each (unwrap-map set) (lambda (pair)
                               (funcall function (car pair)))))

(defmethod iterator ((set set-wrapper))
  (iterator (unwrap-map set)))

(defmethod size ((set set-wrapper))
  (size (unwrap-map set)))

(defmethod compare-objects ((left set-wrapper) (right set-wrapper))
  (compare (unwrap-map left) (unwrap-map right)))

(defmethod check-invariants ((set set-wrapper))
  (check-invariants (unwrap-map set)))

(defmethod map-members ((set set-wrapper) function)
  (rewrap-results set (multiple-value-list (map-members (unwrap-map set) function))))

(defmethod decompose ((set set-wrapper))
  (rewrap-results set (multiple-value-list (decompose (unwrap-map set)))))

(defmethod comparator ((set set-wrapper))
  (comparator (unwrap-map set)))
