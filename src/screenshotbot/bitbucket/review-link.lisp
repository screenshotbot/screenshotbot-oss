;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/bitbucket/review-link
  (:use #:cl)
  (:import-from #:screenshotbot/pro/bitbucket/plugin
                #:bitbucket-repo)
  (:import-from #:screenshotbot/model/channel
                #:github-get-canonical-repo)
  (:import-from #:screenshotbot/dashboard/review-link
                #:get-canonical-pull-request-url)
  (:import-from #:screenshotbot/git-repo
                #:repo-link))
(in-package :screenshotbot/bitbucket/review-link)

(defmethod get-canonical-pull-request-url ((repo bitbucket-repo) pull-request-id)
  (format nil "~a/pull-requests/~d"
          (github-get-canonical-repo
           (repo-link repo))
          pull-request-id))
