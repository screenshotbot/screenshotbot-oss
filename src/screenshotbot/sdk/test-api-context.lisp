;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-api-context
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/api-context
                #:hostname
                #:api-context))
(in-package :screenshotbot/sdk/test-api-context)

(util/fiveam:def-suite)

(def-fixture state ()
  (&body))

(test default-api-hostname

  (is
   (equal "https://api.screenshotbot.io"
          (hostname
           (make-instance 'api-context
                          :hostname nil))))
  (is
   (equal "https://api.screenshotbot.io"
          (hostname
           (make-instance 'api-context
                          :hostname ""))))  
  (is
   (equal "https://api.screenshotbot.io"
          (hostname
           (make-instance 'api-context))))
  (is
   (equal "https://example.com"
          (hostname
           (make-instance 'api-context
                          :hostname "https://example.com")))))
