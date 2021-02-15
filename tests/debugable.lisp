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

(uiop:define-package :pfds.shcl.io/tests/debugable
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/tests/test-interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :prove #:pass #:fail #:skip)
  (:export #:test-debugable))
(in-package :pfds.shcl.io/tests/debugable)

(defun test-graphviz ()
  ;; Check to see if dot is available...
  (handler-case (uiop:run-program "dot"
                                  :input (make-string-input-stream
                                          "digraph {}")
                                  :output (make-broadcast-stream))
    (error ()
      (skip 1 "dot doesn't seem to be working")
      (return-from test-graphviz)))

  ;; Let's see if it barfs when we feed it some of the collection's
  ;; graphs!
  (handler-case
      (progn
        (uiop:run-program "dot"
                          :input (make-string-input-stream
                                  (with-output-to-string (str)
                                    (^graphviz (build-from-list *reverse-sorted-numbers*) str)))
                          :output (make-broadcast-stream))

        (uiop:run-program "dot"
                          :input (make-string-input-stream
                                  (with-output-to-string (str)
                                    (^graphviz (build) str)))
                          :output (make-broadcast-stream)))
    (error (e)
      (fail (format nil "Invalid graphviz document produced: ~A" e))
      (return-from test-graphviz)))

  (pass "Valid graphviz documents produced!"))

(defun test-check-invariants ()
  (^check-invariants (^representative-empty))
  (^check-invariants (build-from-list *unequal-objects*))
  (pass "CHECK-INVARIANTS didn't barf!"))

(defun test-debugable ()
  (named-subtests
    (test-graphviz)
    (test-check-invariants)))
