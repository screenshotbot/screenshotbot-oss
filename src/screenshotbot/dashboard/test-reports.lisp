;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-reports
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/reports
                #:submit-share-report)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/model/sharing
                #:share)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-reports)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store (:globally t)
    (with-test-user (:company company :user user
                     :logged-in-p t)
      (let ((report (make-instance 'report
                                   :company company)))
       (&body)))))

(test submit-share-report
  (with-fixture state ()
    (catch 'hunchentoot::handler-done
      (submit-share-report report nil))

    (let ((shares (class-instances 'share)))
      (assert-that shares
                   (has-length 1)))))
