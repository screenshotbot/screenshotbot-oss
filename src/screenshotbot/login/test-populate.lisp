;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-populate
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/login/populate
                #:populate-company)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:screenshotbot/model/report
                #:report-channel)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:fiveam-matchers/lists
                #:contains))
(in-package :screenshotbot/login/test-populate)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company)))
      (&body))))

(test simple-populate
  (with-fixture state ()
    (populate-company company)))

(test ensure-report-channel-is-populated
  (with-fixture state ()
    (populate-company company)
    (let ((reports (bknr.datastore:class-instances 'report)))
      (assert-that (mapcar #'report-channel reports)
                   (contains
                    (has-typep 'channel))))))
