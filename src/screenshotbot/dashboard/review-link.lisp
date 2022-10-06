;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/review-link
  (:use #:cl)
  (:import-from #:screenshotbot/model/recorder-run
                #:gitlab-merge-request-iid
                #:phabricator-diff-id)
  (:import-from #:screenshotbot/user-api
                #:pull-request-url
                #:recorder-run-channel
                #:channel-repo)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/gitlab/repo
                #:repo-link)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:review-link))
(in-package :screenshotbot/dashboard/review-link)

(markup:enable-reader)

(defun review-link (&key run)
  (cond
    ((phabricator-diff-id run)
     <a href= (pull-request-url run) >Revision</a>)
    ((gitlab-merge-request-iid run)
     (let ((repo (?. channel-repo (?. recorder-run-channel run))))
       (when repo
         <a href= (format nil "~a/-/merge_requests/~a"
                   (repo-link repo)
                                   (gitlab-merge-request-iid run))
            >
           Merge Request
         </a>)))
    ((pull-request-url run)
     <a href= (pull-request-url run)>Pull Request</a>)))
