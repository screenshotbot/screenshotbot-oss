;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :deadbeef
  :serial t
  :depends-on ()
  :components ((:file "util")
               (:file "impl")
               (:file "all")))

(defsystem :deadbeef/tests
  :serial t
  :depends-on (:deadbeef
               :pkg
               :fiveam)
  :components ((:file "test-impl")))
