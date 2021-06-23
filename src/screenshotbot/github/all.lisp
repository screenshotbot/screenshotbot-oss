;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/github
    (:use #:cl
          #:alexandria)
  (:use-reexport #:./github/marketplace
                 #:./github/webhook
                 #:./github/access-checks
                 #:./github/pr-checks
                 #:./github/pull-request-promoter
                 #:./github/plugin))
