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

(defpackage :pfds.shcl.io/utility/iterator-tools
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:iterator)
  (:export
   #:iterator-flatten #:iterator-flatten*))
(in-package :pfds.shcl.io/utility/iterator-tools)

(defun iterator-flatten (iterator)
  (let (current-iterator)
    (labels
        ((iterate ()
           (loop
             (unless current-iterator
               (multiple-value-bind (new-iterator valid-p) (funcall iterator)
                 (unless valid-p
                   (return-from iterate (values nil nil)))
                 (assert new-iterator)
                 (setf current-iterator new-iterator)))
             (multiple-value-bind (result valid-p) (funcall current-iterator)
               (if valid-p
                   (return-from iterate (values result valid-p))
                   (setf current-iterator nil))))))
      #'iterate)))

(defun iterator-flatten* (&rest iterators)
  (iterator-flatten (iterator iterators)))
