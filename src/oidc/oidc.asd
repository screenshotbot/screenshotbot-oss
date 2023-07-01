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
               #:cl-json
               #:dexador
               #:pkg)
  :components ((:file "oidc")
               (:file "all")))

(defsystem :oidc/tests
  :serial t
  :depends-on (#:oidc
               #:util/fiveam
               #:cl-mock)
  :components ((:file "test-oidc")))
