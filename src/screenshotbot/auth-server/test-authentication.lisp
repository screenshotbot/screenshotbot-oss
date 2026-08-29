;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-authentication
  (:use #:cl
        #:fiveam)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/auth-server/authentication
                #:resource-mismatch-error
                #:%authenticate-request)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/auth-server/model
                #:oauth-access-token))
(in-package :screenshotbot/auth-server/test-authentication)


(util/fiveam:def-suite)

(def-fixture state (&key (script-name "/mcp"))
  (with-test-store ()
    (with-fake-request (:script-name script-name)
     (&body))))

(test actually-authenticate-happy-path
  (with-fixture state ()
    (let ((api-key (make-instance 'oauth-access-token
                                  :resource "https://example.com/mcp")))
      (finishes
        (%authenticate-request hunchentoot:*request* api-key)))))

(test fails-to-authenticate-/api-endpoints
  (with-fixture state (:script-name "/api/runs")
    (let ((api-key (make-instance 'oauth-access-token
                                  :resource "https://example.com/mcp")))
      (signals resource-mismatch-error
        (%authenticate-request hunchentoot:*request* api-key)))))

(test authenticates-if-resource-is-not-provided
  (with-fixture state (:script-name "/mcp")
    (let ((api-key (make-instance 'oauth-access-token)))
      (finishes
        (%authenticate-request hunchentoot:*request* api-key)))))

