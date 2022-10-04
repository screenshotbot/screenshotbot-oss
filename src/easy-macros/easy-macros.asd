;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :easy-macros
  :serial t
  :depends-on ()
  :components ((:file "macros")))

(defsystem :easy-macros/tests
  :serial t
  :depends-on (:easy-macros
               :fiveam
               :alexandria
               :fiveam-matchers)
  :components ((:file "test-macros")))
