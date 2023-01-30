;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/review-link
  (:use #:cl)
  (:import-from #:screenshotbot/dashboard/review-link
                #:get-canonical-pull-request-url)
  (:import-from #:screenshotbot/git-repo
                #:repo-link)
  (:import-from #:screenshotbot/github/access-checks
                #:github-repo)
  (:import-from #:screenshotbot/model/channel
                #:github-get-canonical-repo))
(in-package :screenshotbot/github/review-link)

(defmethod get-canonical-pull-request-url ((repo github-repo) pull-request-id)
  (format nil "~a/pulls/~d"
          (github-get-canonical-repo
           (repo-link repo))
          pull-request-id))
