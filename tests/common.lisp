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

(defpackage :pfds.shcl.io/tests/common
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:size #:for-each #:to-list #:empty #:is-empty)
  (:import-from :pfds.shcl.io/utility/compare
   #:compare)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert)
  (:export #:check-common-consistency))
(in-package :pfds.shcl.io/tests/common)

(defun check-empty (object)
  (let ((size (size object)))
    (cassert (zerop size)
             nil "A supposedly empty object had size ~A"
             size))

  (let ((count 0))
    (for-each object (lambda (&rest args)
                       (declare (ignore args))
                       (incf count)))
    (cassert (zerop count)
             nil "for-each says there are ~A objects in a supposedly empty collection"
             count))

  (let ((list (to-list object)))
    (cassert (null list)
             nil "to-list produced a non-empty list on a supposedly empty collection:~A~%"
             list))

  (cassert (eq object (empty object))
           nil "empty on an empty object should return itself"))

(defun check-nonempty (object)
  (let ((size (size object))
        (count 0)
        list
        length)
    (cassert (plusp size)
             nil "A non-empty object had size ~A"
             size)

    (for-each object (lambda (&rest args)
                       (declare (ignore args))
                       (incf count)))
    (cassert (equal count size)
             nil "size and for-each must agree about the size: ~A vs ~A"
             size count)

    (setf list (to-list object))
    (setf length (length list))
    (cassert (equal (length list) size)
             nil "to-list and size must agree about the size: ~A vs ~A"
             size length)

    (cassert (not (eq object (empty object)))
             nil "empty on a non-empty object must return something different")

    (cassert (not (eq :equal (compare object (empty object))))
             nil "empty and non-empty objects don't compare :equal")))

(defun check-write-read-roundtrip (object)
  (let* ((string (with-output-to-string (s) (write object :readably t :stream s)))
         (form (read (make-string-input-stream string)))
         (new-object (eval form)))
    (cassert (eq :equal (compare object new-object))
             nil "Roundtripping through write/read/eval must work")))

(defun check-common-consistency (object)
  (if (is-empty object)
      (check-empty object)
      (check-nonempty object))

  ;; Soon!
  ;;(check-write-read-roundtrip object)
  )
