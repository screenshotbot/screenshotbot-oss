;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :util.threading
  :depends-on (:bordeaux-threads
               :mailbox
               :str
               :fset
               :lparallel
               :atomics
               :log4cl
               :trivial-backtrace
               #- (or screenshotbot-oss eaase-oss)
               :sentry
               :sentry-client
               :trivial-garbage
               :easy-macros
               :util/misc)
  :serial t
  :components ((:file "fake-mp")
               (:file "threading")))

(defsystem :util.threading/tests
  :depends-on (:util.threading
               :fiveam
               :fiveam-matchers
               :util/testing
               :cl-mock
               :alexandria
               :serapeum)
  :serial t
  :components ((:module "tests"
                :components ((:file "test-threading")))))