;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/dashboard/test-recent-runs
    (:use #:cl
          #:alexandria
          #:fiveam
          #:../user-api
          #:../model/github
          #:../model/recorder-run
          #:./recent-runs)
  (:import-from #:./recent-runs
                #:recorder-run-row
                #:render-recent-runs)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:../factory
                #:*user*
                #:*company*
                #:test-company))

(util/fiveam:def-suite)

(defclass test-run ()
  ())

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
  "deadbeef")

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
  (let ((runs (loop for i from 1 to 100 collect
                                        (make-instance 'test-run))))
    (let ((company (make-instance 'test-company :runs runs)))
     (render-recent-runs runs
                         :user *user*
                         :check-access-p nil
                         :script-name "/runs"
                         :numbersp nil
                         :company company)
      (pass))))
