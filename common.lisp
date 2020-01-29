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

(defpackage :pfds.shcl.io/common
  (:use :common-lisp)
  (:export
   #:is-empty #:empty #:with-member
   #:to-list
   #:check-invariants))
(in-package :pfds.shcl.io/common)

(defgeneric to-list (object))

(defgeneric is-empty (container))
(defgeneric empty (container))
(defgeneric with-member (container item))

(defgeneric check-invariants (object))

(defmethod check-invariants (object)
  ;; looks fine to me!
  )
