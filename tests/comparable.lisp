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

(defpackage :pfds.shcl.io/tests/comparable
  (:use :common-lisp)
  (:use :pfds.shcl.io/interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/utility/compare #:compare)
  (:import-from :prove #:is #:ok)
  (:export #:test-comparable))
(in-package :pfds.shcl.io/tests/comparable)

(defun test-comparable ()
  (is (build) (build) :test #'compare-equal-p
      "Empty objects are equal")
  (ok (member (compare (build) (build 1)) '(:less :greater))
      "Empty objects have order compared to non-empty objects")
  (ok (member (compare (build 1) (build 2)) '(:less :greater))
      "Non-empty objects have order compared to each other")
  (ok (member (compare (build-from-list *even-numbers*) (build-from-list *odd-numbers*)) '(:less :greater))
      "Big non-empty objects have order compared to big non-empty objects")
  (is (compare (build-from-list (loop :for i :below *problem-size* :collect (make-token :value i)))
               (build-from-list (loop :for i :below *problem-size* :collect (make-token :value i))))
      :unequal
      "Unequal collections are unequal"))
