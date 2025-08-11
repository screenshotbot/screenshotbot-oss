;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :util.misc
  :serial t
  :depends-on (:local-time
               :cl-fad
               :easy-macros
               :str)
  :components ((:file "misc")
               (:file "lists")))

(defsystem :util.misc/tests
  :depends-on (:util.misc
               :fiveam
               :fiveam-matchers
               :util/fiveam
               :tmpdir
               :alexandria)
  :serial t
  :components ((:file "test-misc")
               (:file "test-lists")))