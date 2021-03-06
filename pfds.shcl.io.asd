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

(defsystem "pfds.shcl.io"
  :class :package-inferred-system
  :description "Purely Functional Data Structures"
  :version "0.0.1"
  :author "Ada Avery <2561504+SquircleSpace@users.noreply.github.com>"
  :licence "Apache License, Version 2.0"
  :depends-on ("pfds.shcl.io/main")
  :in-order-to ((test-op (test-op "pfds.shcl.io/tests"))))

(defsystem "pfds.shcl.io/tests"
  :depends-on ("pfds.shcl.io/tests/main")
  :perform (test-op (o c) (symbol-call '#:pfds.shcl.io/tests/main '#:run-tests)))

(register-system-packages "pfds.shcl.io/main" '(:pfds.shcl.io))
