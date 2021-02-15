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

(uiop:define-package :pfds.shcl.io/utility/printer
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/implementation/interface)
  (:import-from :pfds.shcl.io/utility/specialization
   #:define-specializable-function)
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

(define-specializable-function print-set (<interface>) (set stream)
  (if (i-is-empty <interface> set)
      (write-string "{{}}" stream)
      (pprint-logical-block (stream nil :prefix "{{ " :suffix "}}")
        (i-for-each
         <interface>
         set
         (lambda (obj)
           (write (quote-for-printing obj) :stream stream)
           (write-string " " stream)
           (pprint-newline :fill stream))))))

(define-specializable-function print-map (<interface>) (map stream)
  (if (i-is-empty <interface> map)
      (write-string "{}" stream)
      (pprint-logical-block (stream nil :prefix "{ " :suffix "}")
        (i-for-each
         <interface>
         map
         (lambda (pair)
           (write `(,(quote-for-printing (car pair))
                    ,(quote-for-printing (cdr pair)))
                  :stream stream)
           (write-string " " stream)
           (pprint-newline :fill stream))))))

(define-specializable-function print-sequence (<interface>) (sequence stream)
  (if (i-is-empty <interface> sequence)
      (write-string "[]" stream)
      (pprint-logical-block (stream nil :prefix "[ " :suffix "]")
        (i-for-each
         <interface>
         sequence
         (lambda (obj)
           (write (quote-for-printing obj) :stream stream)
           (write-string " " stream)
           (pprint-newline :fill stream))))))

(define-specializable-function print-container (<interface>) (container stream)
  (print-unreadable-object (container stream :type t)
    (let ((items (i-to-list <interface> container)))
      (pprint-logical-block (stream items)
        (dolist (item items)
          (write item :stream stream)
          (format stream " ")
          (pprint-newline :fill stream))))))
