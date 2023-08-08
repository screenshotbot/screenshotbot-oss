;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-report
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/report
                #:report-to-dto
                #:company-promotion-reports
                #:%report-company)
  (:import-from #:screenshotbot/api/model
                #:encode-json)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:screenshotbot/user-api
                #:user-companies
                #:channel
                #:can-view)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature))
(in-package :screenshotbot/model/test-report)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (run (make-instance 'recorder-run
                               :company company
                               :screenshot-map nil)))
      (&body))))

(test simple-creation ()
  (with-fixture state ()
    (finishes
      (make-instance 'report :acceptable nil))))

(test crashes-on-bad-args ()
  (with-fixture state ()
    (signals #+lispworks conditions:unknown-keyword-error
      #-lispworks error
      (make-instance 'report :does-not-exist-arg t))))

(test sets-company ()
  (with-fixture state ()
    (let ((report (make-instance 'report :run run)))
      (is (eql (%report-company report)
               company)))))

(test company-promoted-index
  (with-fixture state ()
    (let ((report-1 (make-instance 'report :run run
                                           :promotion-report-p t))
          (report-2 (make-instance 'report :run run)))
      (is
       (fset:equal?
        (fset:with (fset:empty-set) report-1)
        (company-promotion-reports company))))))

(test dto-is-serializable
  (with-fixture state ()
    (let ((report (make-instance 'report
                                 :run run
                                 :previous-run run)))
      (finishes
        (encode-json (report-to-dto report))))))

(defclass multi-install (multi-org-feature
                         installation)
  ())

(test can-view-on-report
  (with-fixture state ()
    (with-installation (:installation (make-instance 'multi-install))
     (with-test-user (:user user :company company)
       (let* ((channel (make-instance 'channel :company company))
              (run1 (make-recorder-run :company company
                                       :channel channel))
              (run2 (make-recorder-run :company company
                                       :channel channel))
              (report (make-instance 'report
                                     :run run1
                                     :previous-run run2)))
         (assert-that (user-companies user)
                      (has-item company))
         (is-true user)
         (is-true (can-view report user)))))))

(test can-view-on-report-with-nil-previous
  (with-fixture state ()
    (with-installation (:installation (make-instance 'multi-install))
     (with-test-user (:user user :company company)
       (let* ((channel (make-instance 'channel :company company))
              (run1 (make-recorder-run :company company
                                       :channel channel))
              (run2 nil)
              (report (make-instance 'report
                                     :run run1
                                     :previous-run run2)))
         (assert-that (user-companies user)
                      (has-item company))
         (is-true user)
         (is-true (can-view report user)))))))
