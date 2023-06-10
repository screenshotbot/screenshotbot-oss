;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webhook/test-settings
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-test-user
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/webhook/settings
                #:get-webhook-settings)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:auth
                #:with-sessions)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/webhook/test-settings)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
   (with-installation ()
     (with-test-user (:logged-in-p t)
       (with-sessions ()
         (&body))))))

(screenshot-test webhook-settings-page
  (with-fixture state ()
    (get-webhook-settings)))
