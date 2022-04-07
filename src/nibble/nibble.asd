;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :nibble
  :serial t
  :depends-on (:log4cl
               :auth
               :cl-cron
               :hunchentoot-extensions
               :secure-random)
  :components ((:file "package")
               (:file "nibble")))

(defsystem :nibble/tests
  :serial t
  :depends-on (:nibble
                  :util/testing
                  :util/fiveam
                  :fiveam)
  :components ((:file "test-nibble")))
