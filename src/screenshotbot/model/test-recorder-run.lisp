;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-recorder-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/recorder-run
                #:channel-runs
                #:make-recorder-run
                #:pull-request-id
                #:transient-promotion-log
                #:promotion-log
                #:recorder-run)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:equal-to
                #:has-typep
                #:assert-that)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-commit
                #:channel)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:screenshotbot/model/channel
                #:production-run-for)

  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-recorder-run)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((run (make-recorder-run))
          (promotion-log (make-instance 'promotion-log)))
     (&body))))


(test promotion-log-for-new
  (with-fixture state ()
    (assert-that (promotion-log run)
                 (has-typep 'transient-promotion-log))
    (is (pathnamep
         (blob-pathname (promotion-log run))))))

(test pull-request-id
  (with-fixture state ()
    (let ((run2 (make-recorder-run
                 :pull-request "https://foo/bar/20")))
      (is (eql nil (pull-request-id run)))
      (is (eql 20 (pull-request-id run2))))))

(test maintains-channel-runs
  (with-fixture state ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run
                 :commit-hash "bleh2"
                 :channel channel)))
      (assert-that (production-run-for channel :commit "car")
                   (has-length 0))
      (assert-that (channel-runs channel)
                   (contains run))
      (assert-that (production-run-for channel :commit "bleh2")
                   (is-equal-to run))
      (bknr.datastore:delete-object run)
      (assert-that (channel-runs channel)
                   (has-length 0))
      (assert-that (production-run-for channel :commit "bleh2")
                   (has-length 0)))))

(test if-commit-is-not-present-try-override-commit
  (with-fixture state ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run
                 :override-commit-hash "bleh")))
      (is (equal "bleh" (recorder-run-commit run))))))

(test if-both-commit-and-override-commit-is-not-present
  "Then we return nil without failing"
  (with-fixture state ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run)))
      (is (equal nil (recorder-run-commit run))))))
