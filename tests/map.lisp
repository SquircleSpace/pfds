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

(defpackage :pfds.shcl.io/tests/map
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/common
   #:to-list #:check-invariants)
  (:import-from :pfds.shcl.io/immutable-structure
   #:define-immutable-structure)
  (:import-from :pfds.shcl.io/compare
   #:compare #:compare-objects)
  (:import-from :pfds.shcl.io/map
   #:is-empty #:empty #:lookup-entry #:with-entry #:without-entry)
  (:import-from :pfds.shcl.io/set
   #:with-member #:without-member #:is-member)
  (:import-from :pfds.shcl.io/red-black-tree
   #:make-red-black-map*)
  (:import-from :pfds.shcl.io/unbalanced-tree
   #:make-unbalanced-map*)
  (:import-from :pfds.shcl.io/tests/set #:test-set)
  (:import-from :prove #:is #:subtest #:ok #:pass #:fail))
(in-package :pfds.shcl.io/tests/map)

(define-immutable-structure set-wrapper
  (map (error "required")))

(defun wrap (map)
  (make-set-wrapper :map map))

(defun unwrap (set)
  (set-wrapper-map set))

(defmethod is-empty ((set set-wrapper))
  (is-empty (unwrap set)))

(defmethod empty ((set set-wrapper))
  (wrap (empty (unwrap set))))

(defmethod with-member ((set set-wrapper) key)
  (let ((result (with-entry (unwrap set) key nil)))
    (if (eq result (unwrap set))
        set
        (wrap result))))

(defmethod without-member ((set set-wrapper) key)
  (let ((result (without-entry (unwrap set) key)))
    (if (eq result (unwrap set))
        set
        (wrap result))))

(defmethod is-member ((set set-wrapper) key)
  (nth-value 1 (lookup-entry (unwrap set) key)))

(defmethod to-list ((set set-wrapper))
  (mapcar 'car (to-list (unwrap set))))

(defmethod check-invariants ((set set-wrapper))
  (check-invariants (unwrap set)))

(defun wrapped-set-constructor (map-constructor)
  (lambda (&optional items)
    (let ((alist (mapcar (lambda (i) (cons i nil)) items)))
      (wrap (funcall map-constructor :alist alist)))))

(defun test-map (constructor)
  (subtest "set-like behavior"
    (test-set (wrapped-set-constructor constructor))))

(defun constructor (maker)
  (lambda (&key alist plist)
    (funcall maker 'compare :alist alist :plist plist)))

(defparameter *makers*
  '(make-red-black-map*
    make-unbalanced-map*))

(defun run-tests (&optional (makers *makers*))
  (dolist (maker makers)
    (subtest (symbol-name maker)
      (test-map (constructor maker)))))