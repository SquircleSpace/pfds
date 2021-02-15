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

(uiop:define-package :pfds.shcl.io/tests/collection
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/tests/test-interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/utility/misc #:cassert)
  (:import-from :pfds.shcl.io/utility/impure-list-builder
   #:make-impure-list-builder #:impure-list-builder-add #:impure-list-builder-extract)
  (:import-from :pfds.shcl.io/utility/iterator-tools #:iterator-to-list)
  (:import-from :prove #:is #:isnt #:ok #:pass #:fail #:skip)
  (:export #:test-collection))
(in-package :pfds.shcl.io/tests/collection)

(defun test-empty ()
  (let ((e (build)))
    (is (^empty e) e
        :test #'eql
        "EMPTY doesn't modify empty inputs")

    (ok (^is-empty e)
        "Empty is empty")

    (is (^to-list e) nil
        "Empty is empty")

    (is (multiple-value-list (^decompose e)) (list e nil nil)
        :test #'equal
        "DECOMPOSE on an empty instance returns the container and two nil values")

    (is (^compare e (build)) :equal
        "Empty instances are equal")

    (ok (member (^compare e (build 1)) '(:less :greater))
          "Empty instances aren't equal to non-empty instances")

    (let (trip-wire)
      (is (^map-members e (lambda (i) (setf trip-wire t) i)) e
          :test #'eql
          "MAP-MEMBERS on an empty collection should return the input")
      (is trip-wire nil
          "MAP-MEMBERS on an empty collection shouldn't call the function"))

    (let (trip-wire)
      (^for-each e (lambda (i) (setf trip-wire t) i))
      (is trip-wire nil
          "FOR-EACH on an empty collection shouldn't call the function"))

    (let ((it (^iterator e)))
      (is (multiple-value-list (funcall it)) '(nil nil)
          "Iterating an empty collection signals emptiness immediately")
      (is (multiple-value-list (funcall it)) '(nil nil)
          "... and it doesn't change its mind"))

    (ok (member (^compare e (^with-member e (encode 1 e))) '(:less :greater))
          "Adding something to a collection should make it not-equal to empty")

    (is (^empty (build 1)) e
        :test #'^compare-equal-p
        "EMPTYing out a non-empty collection should produce something empty")

    (^check-invariants e)))

(defun list-using-for-each (collection)
  (let ((builder (make-impure-list-builder)))
    (^for-each collection (lambda (i) (impure-list-builder-add builder i)))
    (impure-list-builder-extract builder)))

(defun list-using-map-members (collection)
  (let* ((builder (make-impure-list-builder)))
    ;; We're kind of breaking the rules by using MAP-MEMBERS for side
    ;; effects.  It can be lazy if it wants.  So... we need to force
    ;; the result to be fully evaluated.
    (^for-each (^map-members collection (lambda (i) (impure-list-builder-add builder i) i))
               (constantly nil))
    (impure-list-builder-extract builder)))

(defun list-using-decompose (collection)
  (let ((builder (make-impure-list-builder)))
    (loop :until (^is-empty collection) :do
      (multiple-value-bind (new-collection value valid-p) (^decompose collection)
        (cassert valid-p)
        (impure-list-builder-add builder value)
        (setf collection new-collection)))
    (impure-list-builder-extract builder)))

(defun test-traversal-for-collection (list comment)
  (let* ((collection (build-from-list list))
         (to-list-list (^to-list collection))
         (iterator-list (iterator-to-list (^iterator collection)))
         (for-each-list (list-using-for-each collection))
         (map-members-list (list-using-map-members collection))
         (decompose-list (list-using-decompose collection)))
    (is iterator-list to-list-list
        (format nil "ITERATOR and TO-LIST must agree (~A)" comment))
    (is for-each-list to-list-list
        (format nil "FOR-EACH and TO-LIST must agree (~A)" comment))
    (is map-members-list to-list-list
        (format nil "MAP-MEMBERS and TO-LIST must agree (~A)" comment))

    ;; decompose is allowed to return objects in a different
    ;; order... but it should have the same objects!
    (let ((table (make-hash-table :test #'equal)))
      (dolist (item decompose-list)
        (incf (gethash item table 0)))
      (dolist (item to-list-list)
        (decf (gethash item table 0)))
      (let ((decompose-extras (loop :for key :being :the :hash-keys :of table :using (:hash-value value)
                                    :when (plusp value) :collect key))
            (to-list-extras (loop :for key :being :the :hash-keys :of table :using (:hash-value value)
                                  :when (minusp value) :collect key)))
        (is decompose-extras
            nil
            (format nil "DECOMPOSE emits no extra members compared to TO-LIST (~A)" comment))
        (is to-list-extras
            nil
            (format nil "DECOMPOSE has all the members that TO-LIST does (~A)" comment))))))

(defun test-traversal ()
  (test-traversal-for-collection *random-numbers* "random")
  (test-traversal-for-collection *unequal-objects* "unequal-objects")
  (test-traversal-for-collection nil "empty"))

(defun test-size ()
  (is (^size (build)) 0
      "Empty instances have 0 size")
  (is (^size (build 1)) 1
      "Non-empty instances have non-zero size")
  (is (^size (build-from-list *even-numbers*)) *problem-size*
      "Big instances have the right size")
  (is (^size (^decompose (build-from-list *even-numbers*))) (1- *problem-size*)
      "Removing an object from the collection decreases the size")
  (is (^size (^with-member (build-from-list *even-numbers*) (encode 1 (build)))) (1+ *problem-size*)
      "Adding an object to a collection increases the size"))

(defun print-read-roundtrip (object)
  (read-from-string (with-output-to-string (str) (write object :stream str :readably t))))

(defun test-print-read ()
  (let* ((e (build))
         (full (build-from-list (concatenate 'list (loop :for i :below 20 :collect i) *random-numbers*))))
    (is (print-read-roundtrip e) e :test #'^compare-equal-p
        "Empty collections are readable")
    (is (print-read-roundtrip full) full :test #'^compare-equal-p
        "Full collections are readable")))

(defun tweak (object)
  (etypecase object
    (number
     (1+ object))
    (character
     (code-char (mod (1+ (char-code object)) char-code-limit)))))

(defun test-map-members ()
  (let* ((e (build))
         (tweaker (lambda (obj) (encode (tweak (decode obj e)) e))))
    (is (^map-members (build) tweaker) (build) :test #'^compare-equal-p
        "MAP-MEMBERS on an empty collection returns an empty collection")
    (is (^map-members (build-from-list *even-numbers*) tweaker)
        (build-from-list (mapcar #'tweak *even-numbers*))
        :test #'^compare-equal-p
        "MAP-MEMBERS on a non-empty collection returns the right value")))

(defun test-build-up-and-tear-down ()
  (let ((c (build))
        (table (make-hash-table :test #'equal))
        (count 0))
    (dolist (item *unequal-objects*)
      (setf item (encode item c))
      (let ((new-c (^with-member c item)))
        (incf count)
        (^check-invariants new-c)
        (incf (gethash item table 0))
        (cassert (equal (^size new-c) count) nil
                 "Collection should have the right size")
        (cassert (equal (length (^to-list new-c)) count) nil
                 "Collection should have the right size")
        (setf c new-c)))

    (pass "Collection survived build-up phase")

    (dotimes (i count)
      (let ((values (multiple-value-list (^decompose c))))
        (destructuring-bind (new-c extracted-value valid-p) values
          (cassert valid-p nil
                   "Collection should remain valid until all items have been removed")
          (decf (gethash extracted-value table 0))
          (decf count)
          (^check-invariants new-c)
          (cassert (equal (^size new-c) count) nil
                   "Collection should have the right size")
          (cassert (equal (length (^to-list new-c)) count) nil
                   "Collection should have the right size")
          (setf c new-c))))

    (let ((values (multiple-value-list (^decompose c))))
      (is values (list c nil nil)
          "DECOMPOSE-ing an empty collection produces the right result"))

    (is (loop :for key :being :the :hash-keys :of table :using (:hash-value value)
              :unless (equal 0 value) :collect key)
        nil
        "All objects are accounted for")))

(defun test-collection ()
  (named-subtests
    (test-empty)
    (test-traversal)
    (test-print-read)
    (test-map-members)
    (test-build-up-and-tear-down)))
