;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-batch
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/batch
                #:batch-item
                #:batch)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/dashboard/batch
                #:batch-item-link
                #:render-batch)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/report-api
                #:report))
(in-package :screenshotbot/dashboard/test-batch)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-installation ()
     (with-fake-request ()
       (let* ((company (make-instance 'company))
              (batch (make-instance 'batch))
              (run (make-recorder-run))
              (report (make-instance 'report)))
         (&body))))))

(screenshot-test batch-item-empty-view
  (with-fixture state ()
    (render-batch batch)))

(screenshot-test batch-item-with-few-batches
  (with-fixture state ()
    (let ((channel (make-instance 'channel
                                  :company company
                                  :name "//foo:bar")))
      (make-instance 'batch-item
                     :run run
                     :channel channel
                     :batch batch)
      (render-batch batch))))


(test batch-item-link
  (with-fixture state ()
    (is (not (str:emptyp
              (batch-item-link
               (make-instance 'batch-item
                              :run run)))))
    (is (not (str:emptyp
              (batch-item-link
               (make-instance 'batch-item
                              :run report)))))))
