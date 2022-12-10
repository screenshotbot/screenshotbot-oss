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
  (:import-from #:nibble
                #:nibble)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:review-link
   #:review-link-impl))
(in-package :screenshotbot/dashboard/review-link)

(markup:enable-reader)

(defun bad-url-page (url)
  <simple-card-page>
    <div class= "card-header">
      <h3>Invalid URL for Pull Request</h3>
    </div>

    <div class= "card-body">
      <p>The Pull Request URL is passed using the <tt>--pull-request</tt> command line
        argument. The provided URL <tt>,(progn url)</tt> appears to be invalid.</p>

      <p>On most CI platforms (with an exception of Jenkins)
        the pull request URL can be auto-detected correctly
        and does not need to be provided. </p>
    </div>
  </simple-card-page>)

(defun validate-url (url)
  (cond
    ((str:starts-with-p "http"
                        (quri:uri-scheme (quri:uri url)))
     url)
    (t
     (nibble (:name :invalid-pull-request)
       (bad-url-page url)))))

(defun review-link (&key run)
  (cond
    ((pull-request-url run)
     <a href= (validate-url (pull-request-url run)) >Pull Request</a>)
    (t
     (review-link-impl (?. channel-repo (recorder-run-channel run)) run))))

(defmethod review-link-impl (repo run)
  nil)
