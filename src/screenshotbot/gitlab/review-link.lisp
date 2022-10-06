;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/review-link
  (:use #:cl)
  (:import-from #:screenshotbot/dashboard/review-link
                #:review-link-impl)
  (:import-from #:screenshotbot/gitlab/repo
                #:repo-link
                #:gitlab-repo)
  (:import-from #:screenshotbot/model/recorder-run
                #:gitlab-merge-request-iid)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/gitlab/review-link)

(markup:enable-reader)

(defmethod review-link-impl ((repo gitlab-repo) run)
  (when (gitlab-merge-request-iid run)
    <a href= (format nil "~a/-/merge_requests/~a"
              (repo-link repo)
                              (gitlab-merge-request-iid run))
       >
      Merge Request
    </a>))
