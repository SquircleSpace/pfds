;; Copyright 2019 Ada Avery
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

(defpackage :pfds.shcl.io/interface/set
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:is-empty #:empty #:with-member #:define-interface)
  (:export
   #:set
   #:with-member
   #:without-member
   #:is-member
   #:is-empty
   #:empty))
(in-package :pfds.shcl.io/interface/set)

(define-interface set
  is-empty
  empty

  with-member

  (defgeneric without-member (set object)
    (:documentation
     "Return a new set where the given object has been removed."))

  (defgeneric is-member (set object)
    (:documentation
     "Returns non-nil if the given object is a member of the set.")))
