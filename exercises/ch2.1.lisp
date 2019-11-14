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

(defpackage :pfds.shcl.io/exercises/ch2.1
  (:use :common-lisp :pfds.shcl.io/list))
(in-package :pfds.shcl.io/exercises/ch2.1)

(defun suffixes (list)
  (if (is-empty list)
      (with-head list (empty list))
      (with-head list (suffixes (tail list)))))
