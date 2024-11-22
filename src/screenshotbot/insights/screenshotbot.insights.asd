;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :screenshotbot.insights
  :serial t
  :depends-on (:screenshotbot)
  :components ((:file "variables")
               (:file "date")
               (:file "maps")
               (:file "runs")
               (:file "pull-requests")
               (:file "dashboard")
               (:file "fuzz")))

(defsystem :screenshotbot.insights/tests
  :serial t
  :depends-on (:screenshotbot.insights
               :util/fiveam)
  :components ((:file "test-date")))
