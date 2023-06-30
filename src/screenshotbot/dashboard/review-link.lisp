;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/review-link
  (:use #:cl)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-work-branch
                #:pull-request-id
                #:gitlab-merge-request-iid
                #:phabricator-diff-id)
  (:import-from #:screenshotbot/user-api
                #:pull-request-url
                #:recorder-run-channel
                #:channel-repo)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:screenshotbot/model/channel
                #:github-get-canonical-repo)
  (:import-from #:screenshotbot/git-repo
                #:repo-link)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:review-link
   #:review-link-impl
   #:reunder-pull-request-link
   #:describe-pull-request
   #:get-canonical-pull-request-url))
(in-package :screenshotbot/dashboard/review-link)

(markup:enable-reader)

(defun review-link (&key run)
  (cond
    ((pull-request-id run)
     (render-pull-request-link
      (?. channel-repo (recorder-run-channel run))
      run))
    (t
     (review-link-impl (?. channel-repo (recorder-run-channel run)) run))))

(defmethod review-link-impl (repo run)
  nil)

(defgeneric get-canonical-pull-request-url (repo pull-request-id)
  (:method (repo pull-request-id)
    "#"))

(defmethod render-pull-request-link (repo run)
  (let ((href (get-canonical-pull-request-url
               repo
               (pull-request-id run))))
    <a href=href >,(describe-pull-request repo run)</a>))

(defmethod describe-pull-request (repo run)
  "Pull Request")

(defmethod describe-pull-request :around (repo run)
  (str:concat
   (call-next-method)
   (let ((branch (recorder-run-work-branch run)))
     (unless (str:emptyp branch)
       (format nil " (~a)" (car (last (str:split "/" branch))))))))
