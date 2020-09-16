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

(defpackage :pfds.shcl.io/tests/ordered-map
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/tests/set-wrapper #:set-wrapper)
  (:import-from :pfds.shcl.io/tests/set #:test-set)
  (:import-from :prove #:is #:pass #:fail)
  (:export #:test-ordered-map))
(in-package :pfds.shcl.io/tests/ordered-map)

(defun test-maker-key-shadowing ()
  (let ((map (make :alist '((1 . 2) (3 . 4) (1 . 5))
                   :plist '(6 7 8 9 6 10))))
    (is (lookup-entry map 1)
        2
        "First alist entry is respected")
    (is (lookup-entry map 6)
        7
        "First plist entry is respected")))

(defun ensure-plist (list)
  (if (zerop (mod (length list) 2))
      list
      (list* (car list) 123 (cdr list))))

(defun test-missing-keys ()
  (let ((map (make :plist (ensure-plist *even-numbers*))))
    (check-invariants map)

    (dolist (odd *odd-numbers* (pass "Missing keys return the expected values"))
      (let ((result (multiple-value-list (lookup-entry map odd))))
        (unless (equal result '(nil nil))
          (return (fail (format nil "Looking up a missing key returned unexpected values: ~W" result))))))))

(defun test-set-behaviors ()
  (let ((maker *maker*))
    (with-test-environment (make-test-environment 'set-wrapper :maker maker)
      (test-set))))

(defun test-ordered-map ()
  (named-subtests
    (test-set-behaviors)
    (test-maker-key-shadowing)
    (test-missing-keys)))
