;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defsystem :screenshotbot.figma
  :serial t
  :depends-on (:screenshotbot
               :util/request)
  :components ((:file "figma")
               (:file "view")))

(defsystem :screenshotbot.figma/tests
  :serial t
  :depends-on (:screenshotbot.figma
               :fiveam
               :screenshotbot/testing-lib
               :util/testing)
  :components ((:file "test-view")))



