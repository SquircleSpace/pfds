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

(defpackage :pfds.shcl.io/tests/main
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/tests/heap)
  (:import-from :pfds.shcl.io/tests/set)
  (:import-from :pfds.shcl.io/tests/map)
  (:import-from :pfds.shcl.io/tests/compare)
  (:import-from :prove #:subtest #:*suite* #:suite #:finalize)
  (:export #:run-tests #:main))
(in-package :pfds.shcl.io/tests/main)

(defun run-tests ()
  (let ((*suite* (make-instance 'suite :plan nil)))
    (subtest "Heaps"
      (pfds.shcl.io/tests/heap:run-tests))
    (subtest "Sets"
      (pfds.shcl.io/tests/set:run-tests))
    (subtest "Maps"
      (pfds.shcl.io/tests/map:run-tests))
    (subtest "Compare"
      (pfds.shcl.io/tests/compare:run-tests))
    (finalize)))

(defun main ()
  (if (run-tests)
      (uiop:quit 0)
      (uiop:quit 1)))
