;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/gitlab/plugin
  (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/plugin
                #:plugin
                #:plugin-parse-repo)
  (:import-from #:screenshotbot/gitlab/repo
                #:gitlab-repo)
  (:import-from #:screenshotbot/model/channel
                #:github-repo)
  (:export
   #:gitlab-plugin
   #:gitlab-access-token))
(in-package :screenshotbot/gitlab/plugin)

(defclass gitlab-plugin (plugin)
  ((access-token :initarg :access-token
                 :accessor gitlab-access-token)))
