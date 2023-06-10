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
                #:encode-json))
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
