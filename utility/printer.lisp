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
   #:for-each #:is-empty #:to-list)
  (:export
   #:print-set #:print-map #:print-sequence
   #:print-container))
(in-package :pfds.shcl.io/utility/printer)

(defun quote-for-printing (object)
  (typecase object
    (keyword
     object)
    (symbol
     `',object)
    (cons
     `',object)
    (t
     object)))

(defun print-set (set stream)
  (if (is-empty set)
      (write-string "{{}}" stream)
      (pprint-logical-block (stream nil :prefix "{{ " :suffix "}}")
        (for-each set (lambda (obj)
                        (write (quote-for-printing obj) :stream stream)
                        (write-string " " stream)
                        (pprint-newline :fill stream))))))

(defun print-map (map stream)
  (if (is-empty map)
      (write-string "{}" stream)
      (pprint-logical-block (stream nil :prefix "{ " :suffix "}")
        (for-each map (lambda (pair)
                        (write `(,(quote-for-printing (car pair))
                                 ,(quote-for-printing (cdr pair)))
                               :stream stream)
                        (write-string " " stream)
                        (pprint-newline :fill stream))))))

(defun print-sequence (sequence stream)
  (if (is-empty sequence)
      (write-string "[]" stream)
      (pprint-logical-block (stream nil :prefix "[ " :suffix "]")
        (for-each sequence (lambda (obj)
                           (write (quote-for-printing obj) :stream stream)
                           (write-string " " stream)
                           (pprint-newline :fill stream))))))

(defun print-container (container stream)
  (print-unreadable-object (container stream :type t)
    (let ((items (to-list container)))
      (pprint-logical-block (stream items)
        (dolist (item items)
          (write item :stream stream)
          (format stream " ")
          (pprint-newline :fill stream))))))
