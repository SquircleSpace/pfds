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

(defpackage :pfds.shcl.io/utility/printer
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:for-each #:is-empty)
  (:import-from :pfds.shcl.io/utility/misc
   #:quote-if-symbol)
  (:export
   #:print-set #:print-map))
(in-package :pfds.shcl.io/utility/printer)

(defun print-set (set stream)
  (if (is-empty set)
      (write-string "{{}}" stream)
      (pprint-logical-block (stream nil :prefix "{{ " :suffix "}}")
        (for-each set (lambda (obj)
                        (write (quote-if-symbol obj) :stream stream)
                        (write-string " " stream)
                        (pprint-newline :fill stream))))))

(defun print-map (set stream)
  (if (is-empty set)
      (write-string "{}" stream)
      (pprint-logical-block (stream nil :prefix "{ " :suffix "}")
        (for-each set (lambda (key value)
                        (write `(,(quote-if-symbol key)
                                 ,(quote-if-symbol value))
                               :stream stream)
                        (write-string " " stream)
                        (pprint-newline :fill stream))))))

(defun print-vector (vector stream)
  (if (is-empty vector)
      (write-string "[]" stream)
      (pprint-logical-block (stream nil :prefix "[ " :suffix "]")
        (for-each vector (lambda (obj)
                           (write (quote-if-symbol obj) :stream stream)
                           (write-string " " stream)
                           (pprint-newline :fill stream))))))
