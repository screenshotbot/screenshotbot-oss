;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-verify-email
  (:use #:cl
        #:fiveam)
  (:import-from #:auth/login/verify-email
                #:state
                #:enter-code-screen)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test))
(in-package :screenshotbot/login/test-verify-email)

(util/fiveam:def-suite)

(screenshot-test verify-email-enter-code-screen
  (with-installation ()
   (enter-code-screen
    (make-instance 'state
                   :code 123345
                   :email "foo@example.com"))))
