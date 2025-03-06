;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-report
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/testing
                #:multi-org-test-installation
                #:with-installation
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/api/report
                #:%report-accept)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:screenshotbot/model/report
                #:acceptable-state
                #:base-acceptable)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/test-report)

(util/fiveam:def-suite)

(defclass fake-acceptable (base-acceptable)
  ()
  (:metaclass persistent-class))



(def-fixture state ()
  (with-installation (:installation (make-instance 'multi-org-test-installation))
   (with-test-store ()
     (with-test-user (:logged-in-p t
                      :company company)
       (let* ((channel (make-instance 'channel :company company))
              (run (make-recorder-run  :channel channel
                                       :company company
                                       :screenshots nil))
              (acceptable (make-instance 'fake-acceptable))
              (report (make-instance 'report
                                     :run run
                                     :acceptable acceptable
                                     :previous-run nil)))
         (&body))))))

(test simple-accept-test
  (with-fixture state ()
    (assert-that (%report-accept :oid (oid report))
                 (has-typep 'dto:report))
    (is (eql :accepted (acceptable-state acceptable)))))

(test cannot-access-other-reports
  (with-fixture state ()
    (with-test-user (:logged-in-p t
                     :company-name "other company"
                     :company other-company)
      (is (not (eql other-company company)))
      (is (not (eql company (auth:current-company))))
      (signals auth:no-access-error
       (%report-accept :oid (oid report))))))

