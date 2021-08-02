;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :quick-patch
  :serial t
  :depends-on ()
  :components ((:file "util")
               (:file "impl")
               (:file "all")))

(defsystem :quick-patch/tests
  :serial t
  :depends-on (:quick-patch
               :pkg
               :fiveam)
  :components ((:file "test-impl")))
