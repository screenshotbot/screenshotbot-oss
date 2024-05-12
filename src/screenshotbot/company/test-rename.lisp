;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/company/test-rename
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:screenshot-test
                #:with-test-user)
  (:import-from #:screenshotbot/company/rename
                #:general-company-page))
(in-package :screenshotbot/company/test-rename)

(util/fiveam:def-suite)

(screenshot-test company-general-settings
  (with-test-store ()
    (with-test-user (:company company :logged-in-p t)
      (general-company-page))))
