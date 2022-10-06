;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/test-recent-runs
    (:use #:cl
          #:alexandria
          #:fiveam
          #:screenshotbot/user-api
          #:screenshotbot/model/github
          #:screenshotbot/model/recorder-run
          #:screenshotbot/dashboard/recent-runs)
  (:import-from #:screenshotbot/dashboard/recent-runs
                #:recorder-run-row
                #:render-recent-runs)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:screenshotbot/factory
                #:*user*
                #:*company*
                #:test-company)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/model/company
                #:company-with-name)
  (:import-from #:screenshotbot/model/user
                #:user-with-email)
  (:import-from #:screenshotbot/dashboard/recent-runs
                #:find-recent-runs)
  (:import-from #:screenshotbot/login/common
                #:*current-company-override*)
  (:import-from #:screenshotbot/user-api
                #:pull-request-url
                #:company-runs)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:markup
                #:write-html)
  (:import-from #:screenshotbot/model/recorder-run
                #:gitlab-merge-request-iid
                #:phabricator-diff-id))

(util/fiveam:def-suite)

(defclass test-run ()
  ((phabricator-diff-id
    :initform nil
    :reader phabricator-diff-id)
   (merge-request-iid
    :initform nil
    :reader gitlab-merge-request-iid)
   (pull-request-url
    :initform nil
    :reader pull-request-url)))

(defclass test-channel ()
  ())

(defvar *channel* (make-instance 'test-channel))

(defmethod util:oid ((run test-run))
  "foobar")

(defmethod channel-name ((channel test-channel))
  "blah-channel")

(defmethod recorder-run-channel ((run test-run))
  *channel*)

(defmethod recorder-run-commit ((run test-run))
  "quick-patch")

(defmethod created-at ((run test-run))
  (local-time:now))

(defmethod activep ((run test-run))
  t)

(defmethod channel-repo ((run test-channel))
  (make-instance 'github-repo
                  :link
                  "https://github.com/foo/bar.git"))

(defmethod store-object-id ((Run test-run))
  1)

(test simple-recorder-run-row
  (let ((run (make-instance 'test-run)))
    (recorder-run-row :run run)
    (pass)))

(test recent-runs
  (let ((*installation* (make-instance 'installation)))
   (let ((runs (loop for i from 1 to 100 collect
                                         (make-instance 'test-run))))
     (let ((company (make-instance 'test-company :runs runs)))
       (render-recent-runs runs
                           :user *user*
                           :check-access-p nil
                           :script-name "/runs"
                           :numbersp nil
                           :company company)
       (pass)))))

(defun profile-recent-runs (company email)
  (let* ((company (company-with-name company))
         (user (user-with-email email))
         (*current-company-override* company))
    (with-fake-request ()
      (auth:with-sessions ()
       (loop for i from 0 to 10000 do
         (write-html (render-recent-runs (util/lists:head (company-runs company) 50)
                                         :user *user*
                                         :check-access-p nil
                                         :script-name "/runs"
                                         :numbersp nil
                                         :company company)))))))
