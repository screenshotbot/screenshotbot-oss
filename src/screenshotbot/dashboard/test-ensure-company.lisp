;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-ensure-company
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/ensure-company
                #:%new-company)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/dashboard/test-ensure-company)


(util/fiveam:def-suite)

(screenshot-test empty-new-company-for-companyless-user
  (with-installation ()
    (with-fake-request ()
      (auth:with-sessions ()
       (%new-company)))))
