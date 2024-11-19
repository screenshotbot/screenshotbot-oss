;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/test-run-warnings
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:fix-timestamps
                #:with-test-user
                #:screenshot-test)
  (:import-from #:screenshotbot/dashboard/run-page
                #:render-run-page
                #:run-page)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:push-run-warning
                #:make-recorder-run)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/azure/run-warnings
                #:azure-unauthorized-warning))
(in-package :screenshotbot/azure/test-run-warnings)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company
                     :logged-in-p t)
     (let* ((channel (make-instance 'channel :company company))
            (run (make-recorder-run :channel channel
                                    :screenshots nil)))
       (&body)))))

(screenshot-test azure-unauthorized-warning
  (with-fixture state ()
    (push-run-warning run 'azure-unauthorized-warning)
    (fix-timestamps
     (render-run-page run))))

