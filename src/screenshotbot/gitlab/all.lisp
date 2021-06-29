;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/gitlab
    (:use #:cl)
  (:nicknames :screenshotbot.gitlab)
  (:use-reexport #:./gitlab/repo
                 #:./gitlab/merge-request-promoter
                 #:./gitlab/plugin))

(import 'discussion-id)
