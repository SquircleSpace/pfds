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

(defpackage :pfds.shcl.io/interface/map
  (:use :common-lisp)
  (:import-from :pfds.shcl.io/interface/common
   #:is-empty #:empty #:define-interface)
  (:export
   #:is-empty
   #:empty
   #:with-entry
   #:without-entry
   #:lookup-entry))
(in-package :pfds.shcl.io/interface/map)

(define-interface map
  is-empty
  empty

  (defgeneric with-entry (map key value)
    (:documentation
     "Return a map where the given key/value pair has been added."))

  (defgeneric without-entry (map key)
    (:documentation
     "Return a map where the entry for the given key has been removed."))

  (defgeneric lookup-entry (map key)
    (:documentation
     "Return the value associated with the given key in the map.

If the value doesn't exist in the map, this returns two nil values.
If the value does exist, it returns the stored value and a non-nil
value.")))
