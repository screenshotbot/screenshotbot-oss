;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :nursery
  :description "Structured concurrency for threads: a nursery waits for all of its child threads"
  :author "Arnold Noronha <arnold@screenshotbot.io>"
  :license "Mozilla Public License 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (:bordeaux-threads
               :easy-macros
               :trivial-backtrace)
  :components ((:file "nursery")))

(defsystem :nursery/tests
  :serial t
  :depends-on (:nursery
               :fiveam)
  :components ((:file "test-nursery")))
