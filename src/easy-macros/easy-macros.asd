;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :easy-macros
  :description "An easier way to write 90% of your macros"
  :author "Arnold Noronha <arnold@tdrhq.com>"
  :license  "Apache License, Version 2.0"
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
