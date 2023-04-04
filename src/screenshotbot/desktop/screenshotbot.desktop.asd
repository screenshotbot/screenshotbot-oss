;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :screenshotbot.desktop
  :serial t
  :depends-on (:screenshotbot
               #-screenshotbot-oss
               :screenshotbot.pro.css/extended-dashboard
               :clingon)
  :components ((:file "run")
               (:file "init")))

(defsystem :screenshotbot.desktop/tests
  :serial t
  :depends-on (:screenshotbot.desktop
               :screenshotbot/testing-lib
               :util/fiveam)
  :components ((:file "test-run")))
