;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-reports
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/reports
                #:report-page
                #:render-acceptable-history
                #:submit-share-report)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:fix-timestamps
                #:screenshot-test
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
  (:import-from #:screenshotbot/model/report
                #:acceptable-history-item
                #:base-acceptable)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/dashboard/test-reports)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store (:globally t)
    (with-test-user (:company company :user user
                     :logged-in-p t)
      (let ((report (make-instance 'report)))
       (&body)))))

(test submit-share-report
  (with-fixture state ()
    (catch 'hunchentoot::handler-done
      (submit-share-report report nil))

    (let ((shares (class-instances 'share)))
      (assert-that shares
                   (has-length 1)))))

(screenshot-test render-acceptable-history-empty ()
  (with-fixture state ()
    (let ((report (make-instance 'report))
          (acceptable (make-instance 'base-acceptable
                                     :report report)))
      (render-acceptable-history acceptable))))

(screenshot-test render-acceptable-history-non-empty ()
  (with-fixture state ()
    (let ((report (make-instance 'report))
          (acceptable (make-instance 'base-acceptable
                                     :report report
                                     :history (list
                                               (make-instance 'acceptable-history-item
                                                              :state :accepted
                                                              :user user)
                                               (make-instance 'acceptable-history-item
                                                              :state :rejected
                                                              :user user)))))
      (fix-timestamps
       (render-acceptable-history acceptable)))))

(test report-page-happy-path ()
  (with-fixture state ()
    (with-test-user (:user user :company company :logged-in-p t)
     (let* ((channel (make-instance 'channel :company company :name "bleh"))
            (run1 (make-recorder-run :company company :channel channel))
            (run2 (make-recorder-run :company company :channel channel))
            (report (make-instance 'report
                                   :title "1 changes"
                                   :run run1
                                   :previous-run run2)))
       (assert-that
        (markup:write-html
         (report-page :id (oid report)))
        (contains-string "1 changes"))))))
