;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model
  (:use #:cl #:alexandria)
  (:use-reexport
   #:screenshotbot/model/company
   #:screenshotbot/model/user
   #:screenshotbot/model/invite
   #:screenshotbot/model/github
   #:screenshotbot/model/view
   #:screenshotbot/model/core
   #:screenshotbot/model/recorder-run
   #:screenshotbot/model/report
   #:screenshotbot/model/image
   #:screenshotbot/model/channel
   #:screenshotbot/model/screenshot
   #:screenshotbot/model/api-key
   #:screenshotbot/model/commit-graph
   #:screenshotbot/model/test-object))
(in-package :screenshotbot/model)
