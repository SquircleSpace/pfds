;; Copyright 2021 Ada Avery
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

(uiop:define-package :pfds.shcl.io/tests/deque
  (:use :common-lisp)
  (:use :pfds.shcl.io/utility/interface)
  (:use :pfds.shcl.io/tests/test-interface)
  (:use :pfds.shcl.io/tests/common)
  (:import-from :pfds.shcl.io/utility/misc
   #:cassert
   #:string-starts-with-p
   #:string-ends-with-p)
  (:import-from :prove #:is #:ok #:pass)
  (:export #:test-deque))
(in-package :pfds.shcl.io/tests/deque)

(defun wrap (interface)
  (labels
      ((with-x-wrapper (with-fn)
         (lambda (&rest args)
           (let ((result (apply with-fn args)))
             (i-check-invariants interface result)
             result)))

       (without-x-wrapper (peek-fn)
         (lambda (without-fn)
           (lambda (deque &rest args)
             (multiple-value-bind (result head valid-p) (apply without-fn deque args)
               (i-check-invariants interface result)

               (if valid-p
                   (cassert (not (i-is-empty interface deque)) nil "A non-empty deque should always claim validity when removing an element")
                   (cassert (i-is-empty interface deque) nil "An empty deque should always claim non-validity when removing an element"))

               (multiple-value-bind (other-head peek-valid-p) (funcall peek-fn deque)
                 (cassert (eq other-head head)
                          nil
                          "Peeking and removal should return the same value")
                 (cassert (or (and peek-valid-p valid-p)
                              (and (not peek-valid-p) (not valid-p)))
                          nil
                          "Peeking and removal should agree on validity"))

               (values result head valid-p)))))

       (peek-x-wrapper (peek-fn)
         (lambda (deque &rest args)
           (multiple-value-bind (result valid-p) (apply peek-fn deque args)
             (if valid-p
                 (cassert (not (i-is-empty interface deque)) nil "A non-empty queue should always return a true valid-p result for peek")
                 (cassert (i-is-empty interface deque) nil "An empty queue should always return a nil valid-p result for peek"))
             (values result valid-p)))))

    (wrap-interface
     interface
     'pfds.shcl.io/implementation/interface:with-front #'with-x-wrapper
     'pfds.shcl.io/implementation/interface:with-back #'with-x-wrapper

     'pfds.shcl.io/implementation/interface:without-front (without-x-wrapper
                                                              (interface-get interface 'pfds.shcl.io/implementation/interface:peek-front))
     'pfds.shcl.io/implementation/interface:without-back (without-x-wrapper
                                                             (interface-get interface 'pfds.shcl.io/implementation/interface:peek-back))

     'pfds.shcl.io/implementation/interface:peek-front #'peek-x-wrapper
     'pfds.shcl.io/implementation/interface:peek-back #'peek-x-wrapper)))

(defvar *operations*
  ;; Generated randomly with ~2/3 chance of with, 1/3 chancec of without
  '(^with-back ^with-front ^with-back ^without-back ^without-back ^with-front
    ^with-back ^without-back ^with-back ^with-front ^with-back
    ^with-front ^with-front ^with-back ^with-back ^with-back
    ^without-front ^with-back ^with-front ^without-front ^with-front
    ^without-front ^with-back ^with-back ^with-front ^with-back
    ^without-back ^without-front ^with-front ^with-front ^with-back
    ^without-back ^with-front ^with-back ^with-front ^without-back
    ^without-front ^without-back ^with-back ^with-back ^with-back
    ^with-front ^with-back ^without-back ^with-front ^with-front
    ^without-front ^with-front ^with-front ^without-back ^with-front
    ^without-back ^with-back ^with-front ^without-front ^without-back
    ^with-front ^without-front ^without-front ^with-back ^without-front
    ^with-back ^without-front ^without-back ^with-front ^with-back
    ^without-back ^without-front ^with-front ^with-back ^with-back
    ^with-front ^with-front ^with-front ^with-back ^without-back
    ^without-back ^with-front ^with-front ^with-back ^without-front
    ^with-front ^with-back ^with-front ^with-back ^with-front
    ^with-front ^without-front ^with-front ^without-back ^with-front
    ^with-back ^with-back ^with-back ^with-back ^with-front
    ^without-back ^with-front ^with-front ^with-front
    ;; 37 ^with-front, 32 ^with-back
    ;; 14 ^without-front, 17 ^without-back

    ;; Now let's balance it out to get to empty!  We're going to swap
    ;; it, though.  37 - 14 = 23 ^without-back followed by 32 - 17 =
    ;; 15 ^without-front.  The goal here is to force the deque to pull
    ;; elements from across the "middle" line.  We know the back has
    ;; at most 15 elements.  So, if we pull 23 elements from the back
    ;; then we'll definitely grab an item that was originally pushed
    ;; to the front.  We may have already pulled items across the
    ;; middle earlier... but this way we can be sure.

    ^without-back ^without-back ^without-back ^without-back
    ^without-back ^without-back ^without-back ^without-back
    ^without-back ^without-back ^without-back ^without-back
    ^without-back ^without-back ^without-back ^without-back
    ^without-back ^without-back ^without-back ^without-back
    ^without-back ^without-back ^without-back

    ^without-front ^without-front ^without-front ^without-front
    ^without-front ^without-front ^without-front ^without-front
    ^without-front ^without-front ^without-front ^without-front
    ^without-front ^without-front ^without-front))

(define-interface <<bad-deque>> ()
  ;; Only include the functions we absolutely need so its easier to
  ;; verify the instance is correct.
  pfds.shcl.io/implementation/interface:with-back
  pfds.shcl.io/implementation/interface:with-front
  pfds.shcl.io/implementation/interface:without-back
  pfds.shcl.io/implementation/interface:without-front)

(defun bad-with-back (d o)
  (concatenate 'list d (list o)))

(defun bad-with-front (d o)
  (cons o d))

(defun bad-without-back (d)
  (let* (last-element
         (new-d
           (loop :with head = d :while (cdr head)
                 :collect (car head)
                 :do (setf head (cdr head))
                 :finally (setf last-element (car head)))))
    (values new-d last-element (not (null d)))))

(defun bad-without-front (d)
  (values (cdr d) (car d) (not (null d))))

(define-simple-interface-instance <bad-list-deque> <<bad-deque>> bad-)

(defun swap-side (operation)
  (ecase operation
    (^with-front
     '^with-back)
    (^with-back
     '^with-front)
    (^without-front
     '^without-back)
    (^without-back
     '^without-front)))

(defun test-empty-deques ()
  ;; We can assume that the corresponding queue tests have already
  ;; run.  So, we only have to test the interesting new stuff that
  ;; deques provide.
  (let ((d (^make-deque)))
    (is (multiple-value-list (^peek-back d))
        (list nil nil)
        "PEEK-BACK is expected to return two nils on empty deques")
    (is (multiple-value-list (^without-back d))
        (list d nil nil)
        :test #'equal
        "WITHOUT-BACK is expected to return the same deque and two nils when empty")
    (is (^without-back (^make-deque))
        d
        :test #'eq
        "Deques that become empty through removal of the back are also eq to other empty deques")))

(defun test-operations (operations)
  (let ((numbers *sorted-numbers*)
        (d (^make-deque))
        model)
    (dolist (operation operations)
      (cond
        ((string-starts-with-p (symbol-name operation) "^WITH-")
         (let* ((arg (pop numbers))
                (new-d (funcall operation d arg))
                (new-model (let ((*interface* <bad-list-deque>))
                             (funcall operation model arg))))
           (cassert (equal (^to-list new-d) new-model) (d)
                    "The deque should have the same contents as the model list")
           (setf d new-d)
           (setf model new-model)))

        (t
         (multiple-value-bind
               (new-d object valid-p)
             (funcall operation d)
           (multiple-value-bind
                 (new-model model-object model-valid-p)
               (let ((*interface* <bad-list-deque>))
                 (funcall operation model))
             (cassert (equal object model-object) nil
                      "The deque must return the same object as the model list")
             (cassert (equal (not (null valid-p))
                             (not (null model-valid-p)))
                      nil
                      "The deque and the model must agree on validity")
             (cassert (equal (^to-list new-d) new-model) (d)
                      "The deque should have the same contents as the model list")
             (setf d new-d)
             (setf model new-model))))))

    (pass "Messing with the queue produces the right results")))

(defun test-mixed-operations ()
  (test-operations *operations*)
  ;; Now mirrored... because... I don't know.  Maybe it will catch
  ;; something?
  (test-operations (mapcar #'swap-side *operations*)))

(defun test-deque ()
  (let ((*interface* (wrap *interface*)))
    (named-subtests
      (test-empty-deques)
      (test-mixed-operations))))
