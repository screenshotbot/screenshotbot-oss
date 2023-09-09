;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :quick-patch
  :serial t
  :author "Arnold Noronha <arnold@jipr.io>"
  :license  "Mozilla Public License 2.0"
  :version "0.0.1"
  :description "Easily override quicklisp projects without using git submodules"
  :depends-on ()
  :components ((:file "util")
               (:file "impl")
               (:file "all")))

(defsystem :quick-patch/tests
  :serial t
  :depends-on (:quick-patch
               :cl-mock
               :tmpdir
               :fiveam-matchers
               :str
               :fiveam)
  :components ((:file "test-impl")))
