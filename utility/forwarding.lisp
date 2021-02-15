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

(uiop:define-package :pfds.shcl.io/utility/forwarding
  (:use :common-lisp)
  (:import-from :alexandria)
  (:export
   #:assemble-lambda-list
   #:make-forwarding-lambda
   #:make-forwarding-method
   #:make-forwarding-defun))
(in-package :pfds.shcl.io/utility/forwarding)

(defun lambda-list-metrics (lambda-list)
  (multiple-value-bind
        (required optional rest keyword allow-other-keys aux key-p)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore aux))
    (let* ((min-positionals (length required))
           (max-positionals (cond
                              ((or key-p (null rest))
                               (+ min-positionals (length optional)))
                              (rest
                               :infinity)))
           (accepted-keywords (if allow-other-keys
                                  t
                                  (when key-p
                                    (union (list :allow-other-keys)
                                           (mapcar (lambda (arg) (first (first arg))) keyword))))))
      (values min-positionals max-positionals accepted-keywords))))

(defun metric-subset-p (left right)
  (destructuring-bind (l-min-positionals l-max-positionals l-accepted-keywords l-key-p) left
    (declare (ignore l-key-p))
    (destructuring-bind (r-min-positionals r-max-positionals r-accepted-keywords r-key-p) right
      (declare (ignore r-key-p))

       ;; Right must be less picky about minimum positionals
      (unless (<= r-min-positionals l-min-positionals)
        (return-from metric-subset-p nil))

      ;; Right must be less picky about maximum positionals
      (cond
        ((eq :infinity r-max-positionals)
         (return-from metric-subset-p t))
        ((eq :infinity l-max-positionals)
         (return-from metric-subset-p nil))
        ((> l-max-positionals r-max-positionals)
         (return-from metric-subset-p nil)))

      ;; Okay, we're past the positionals.  Let's compare accepted
      ;; keywords!
      (cond
        ((eq t r-accepted-keywords)
         (return-from metric-subset-p t))
        ((eq t l-accepted-keywords)
         (return-from metric-subset-p nil))
        ((set-difference l-accepted-keywords r-accepted-keywords)
         (return-from metric-subset-p nil)))

      ;; A lambda list that says it accepts no keywords (nothing after
      ;; &KEY) will still happily receive a keyword plist as long as
      ;; it starts with :ALLOW-OTHER-KEYS T.  So, there's a difference
      ;; between having an empty set of accepted keyword arguments and
      ;; not having &KEY at all.  The former is more tolerant than the
      ;; latter!  However, this is covered by the fact that the
      ;; metrics are expected to include :ALLOW-OTHER-KEYS in the set
      ;; of accepted keyword arguments.  In this function we're
      ;; interpreting the accepted keyword list very strictly.  If you
      ;; claim to not support :ALLOW-OTHER-KEYS (somehow) then we're
      ;; going to believe it.  It makes this function a lot simpler!
      t)))

(defun lambda-list-subset-p (left right)
  "Returns non-nil iff the RIGHT lambda list can receive any argument
list the LEFT lambda list can."
  (metric-subset-p (multiple-value-list (lambda-list-metrics left))
                   (multiple-value-list (lambda-list-metrics right))))

(defun assemble-lambda-list (&optional required optional rest keyword allow-other-keys aux (key-p keyword))
  `(,@required
    ,@(when optional (list* '&optional optional))
    ,@(when rest (list '&rest rest))
    ,@(when (or keyword key-p) (list* '&key keyword))
    ,@(when allow-other-keys (list '&allow-other-keys))
    ,@(when aux (list* '&aux aux))))

(defun process-lambda-list (lambda-list)
  (multiple-value-bind
        (required optional rest keyword allow-other-keys aux key-p)
      (alexandria:parse-ordinary-lambda-list lambda-list :normalize t)
    (setf aux nil) ;; we don't care about aux

    ;; No matter what, we want all the arguments to have GENSYM names.
    ;; For cosmetic reasons, actually just use uninterned symbols
    ;; instead of literally using GENSYM.  This way Slime's
    ;; visualization of the lambda list is nicer!
    (setf required (loop :for param :in required
                         :collect (make-symbol (symbol-name param))))
    (setf optional (loop :for param-pack :in optional
                         :for param-name = (symbol-name (first param-pack))
                         :collect (list (make-symbol param-name)
                                        nil
                                        (make-symbol (concatenate 'string param-name "-P")))))
    (setf rest (when rest (make-symbol (symbol-name rest))))
    (setf keyword (loop :for param-pack :in keyword
                        :for keyword-sym = (first (first param-pack))
                        :for param-name = (symbol-name (second (first param-pack)))
                        :collect (list (list keyword-sym (make-symbol param-name))
                                       nil
                                       (make-symbol (concatenate 'string param-name "-P")))))

    (values required
            optional
            rest
            keyword
            allow-other-keys
            aux
            key-p)))

(defparameter *max-keyword-branching-factor* 3)

(defun make-forwarding-lambda (prototype-lambda-list funcall-target-form &key (key-implies-rest t) include-method-keyword-leniency)
  "Returns a lambda form which forwards all arguments it receives to
the target funcallable.

This function returns a lambda form that is spiritually similar to
    `(LAMBDA (&REST REST) (APPLY ,FUNCALL-TARGET-FORM REST))

However, this function doesn't *actually* return that because it
carries some non-trivial downsides.  E.g.

- If the call to the lambda isn't inlined, then the use of &REST will
  all but ensure that consing takes place.

- If the lambda gets inlined, the use of APPLY still trips up some
  optimizations and prevents things like compiler macros from
  running.  This behavior was observed in SBCL 2.0.0.

- Debugging tools like Swank+Slime won't be able to provide a useful
  lambda list for the wrapper.

- Even ignoring the above issues, you're out of luck if you wanted to
  make a method forward its arguments to a different function.  The
  method's lambda list must be congruent with the arguments of other
  methods and so you can't use the &REST trick.  Besides, you wouldn't
  be able to specialize the method!

This function builds a lambda form that has a lambda list that is
congruent with the provided one.  It will receive the arguments and
then forward them all along to the target function.

Note that whenever the lambda list has a &REST parameter and the &REST
parameter is bound to a non-nil value, the target function is passed
that &REST parameter with APPLY.  This is true even when there are
&KEY parameters.  This function assumes that if the lambda list
contains a &REST argument then the function is interested in
inspecting the contents of the keyword plist and must receive the
plist in its entirety.

Since &REST is commonly used when people write forwarding code for
keyword parameters, this function will, by default, add a &REST
parameter if there are any &KEY parameters.  Be aware that you might
forward more arguments than you expect.  This is especially true in
the context of a method where the &REST parameter may contain keyword
arguments that are only meaningful to other methods.

If you set INCLUDE-METHOD-KEYWORD-LENIENCY to a non-nil value, the
wrapper lambda will insert an :ALLOW-OTHER-KEYS argument at the front
of the keyword plist before passing it along.  Note that the
INCLUDE-METHOD-KEYWORD-LENIENCY argument is ignored and has no effect
if the lambda list contains &ALLOW-OTHER-KEYS.  This is because the
target function is already assumed to accept all keyword arguments.

If you set KEY-IMPLIES-REST to :NEVER and the prototype lambda list
doesn't have a &REST parameter, then the wrapper will \"manually\"
forward the keyword arguments by checking which ones were provided and
then passing only those keyword arguments to the target function.
This ensures that ONLY the keyword arguments named in the prototype
lambda list will be received by the target function."
  (multiple-value-bind
        (required optional rest keyword allow-other-keys aux key-p)
      (process-lambda-list prototype-lambda-list)
    (assert (null aux))

    (when (and keyword (null rest)
               key-implies-rest)
      (setf rest (make-symbol "REST")))

    (labels
        ((handle-keywords ()
           (cond
             ((or rest (null keyword))
              ;; Just let the &REST handler take care of it!  We're
              ;; not going to touch those keyword arguments!
              (handle-rest))

             (t
              (if (> (length keyword) *max-keyword-branching-factor*)
                  (handle-keywords-with-apply)
                  (handle-keywords-recursively (reverse keyword) nil)))))

         (handle-keywords-recursively (remaining-keywords assumed-arguments)
           (unless remaining-keywords
             (return-from handle-keywords-recursively
               (if assumed-arguments
                   `(funcall ,funcall-target-form ,@required ,@(mapcar #'first optional) ,@assumed-arguments)
                   (handle-optionals))))

           (let ((param-pack (car remaining-keywords)))
             `(if ,(third param-pack)
                  ,(handle-keywords-recursively (cdr remaining-keywords) (list* `',(first (first param-pack))
                                                                                (second (first param-pack))
                                                                                assumed-arguments))
                  ,(handle-keywords-recursively (cdr remaining-keywords) assumed-arguments))))

         (handle-keywords-with-apply ()
           (let ((our-rest (gensym "REST")))
             `(let (,our-rest)
                ,@(loop :for param-pack :in (reverse keyword)
                        :collect
                        `(when ,(third param-pack)
                           (setf ,our-rest (list* ',(first (first param-pack))
                                                  ,(second (first param-pack))
                                                  ,our-rest))))
                ,(handle-rest our-rest))))

         (handle-rest (&optional (rest-sym rest))
           (if rest-sym
               `(if ,rest-sym
                    (apply ,funcall-target-form
                           ,@required
                           ,@(mapcar #'first optional)
                           ,@(when (and include-method-keyword-leniency key-p (not allow-other-keys))
                               '(:allow-other-keys t))
                           ,rest-sym)
                    ,(handle-optionals))
               (handle-optionals)))

         (handle-optionals ()
           (if (null optional)
               `(funcall ,funcall-target-form ,@required)
               `(cond
                  ,@(loop :with reversed-optional = (reverse optional)
                          :with prior-args = (mapcar #'first reversed-optional)
                          :for param-pack :in reversed-optional
                          :for predicate = (third param-pack)
                          :for param-name = (first param-pack)
                          :collect `(,predicate
                                     (funcall ,funcall-target-form ,@required ,@(reverse prior-args)))
                          :do (pop prior-args))
                  (t
                   (funcall ,funcall-target-form ,@required))))))

      `(lambda ,(assemble-lambda-list required optional rest keyword allow-other-keys aux key-p)
         (declare (ignorable ,@(mapcar (lambda (p) (second (first p))) keyword)
                             ,@(mapcar #'third keyword)))
         ,(handle-keywords)))))

(defun make-forwarding-method (gf-name gf-lambda-list target-function-form &key specializers (key-implies-rest t))
  "Produce a DEFMETHOD form for the given GF that forwards arguments to another function.

This is like MAKE-FORWARDING-LAMBDA expect it produces a DEFMETHOD
form."
  (let* ((forwarding-lambda (make-forwarding-lambda
                             gf-lambda-list
                             target-function-form
                             :include-method-keyword-leniency t
                             :key-implies-rest key-implies-rest))
         (forwarding-lambda-list (second forwarding-lambda))
         (forwarding-body (cddr forwarding-lambda))
         (specialized-lambda-list
           (loop :with remaining-specializers = specializers
                 :for param :in forwarding-lambda-list
                 :collect (cond
                            (remaining-specializers
                             (when (member param lambda-list-keywords)
                               (error "Too many specializers"))
                             `(,param ,(pop remaining-specializers)))
                            (t
                             param)))))
    `(defmethod ,gf-name ,specialized-lambda-list
       ,@forwarding-body)))

(defun make-forwarding-compiler-macro (function-name target-function-form)
  "Produce a MAKE-COMPILER-MACRO that forwards arguments to another function.

This is meant to be used as a companion to MAKE-FORWARDING-DEFUN."
  (let ((rest (gensym "REST")))
    `(define-compiler-macro ,function-name (&rest ,rest)
       `(funcall ,',target-function-form ,@,rest))))

(defun make-forwarding-defun (function-name lambda-list target-function-form
                              &key
                                include-method-keyword-leniency
                                (key-implies-rest t)
                                (inline-p t)
                                (compiler-macro-p t))
  "Produce a DEFUN form that forwards arguments to another function.

This is like MAKE-FORWARDING-LAMBDA except it produces a DEFUN form."
  (let ((forwarding-lambda (make-forwarding-lambda
                            lambda-list
                            target-function-form
                            :include-method-keyword-leniency include-method-keyword-leniency
                            :key-implies-rest key-implies-rest)))
    `(progn
       ,@(when compiler-macro-p
           `(,(make-forwarding-compiler-macro function-name target-function-form)))
       ,@(when inline-p
           `((declaim (inline ,function-name))))
       (defun ,function-name ,@(cdr forwarding-lambda)))))
