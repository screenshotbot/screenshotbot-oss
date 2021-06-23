;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model
    (:use #:cl
          #:alexandria)
  (:use-reexport #:./model/company
                 #:./model/user
                 #:./model/invite
                 #:./model/github
                 #:./model/view
                 #:./model/core
                 #:./model/recorder-run
                 #:./model/report
                 #:./model/image
                 #:./model/channel
                 #:./model/screenshot
                 #:./model/api-key
                 #:./model/commit-graph
                 #:./model/test-object))
