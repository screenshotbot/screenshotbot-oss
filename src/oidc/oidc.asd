;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :oidc
  :serial t
  :depends-on (#:util
               #:alexandria
               #:nibble
               #:hunchentoot-extensions
               #:cl-json
               #:util/request
               #:dexador
               #:pkg
               #:hunchentoot
               #:quri)
  :components ((:file "oauth")
               (:file "oidc")
               (:file "all")))

(defsystem :oidc/tests
  :serial t
  :depends-on (#:oidc
               #:util/fiveam
               #:util.testing
               #:cl-mock
               #:fiveam-matchers)
  :components ((:file "test-oauth")
               (:file "test-oidc")))
