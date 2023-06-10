;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webhook/test-webhook
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/webhook/webhook
                #:actually-send-webhook
                #:sign-payload)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:screenshotbot/webhook/model
                #:endpoint
                #:ensure-webhook-config)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:cl-mock
                #:if-called))
(in-package :screenshotbot/webhook/test-webhook)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company)
      (let ((config (ensure-webhook-config company)))
        (with-transaction ()
         (setf (endpoint config)  "fakehttp://example.com"))
        (&body)))))

(test sign-payload
  (is (equal "t=1686193117,signature=5d6ec12164ab98b79d4c53e5ede481c2cd283e2035fca7cc6206a4b275e06056"
             (sign-payload "blehbleh"
                           :time
                           (local-time:universal-to-timestamp
                            3895181917)
                           :key "zoidberg"))))

(test actually-send-webhook-happy-path
  (with-fixture state ()
    (cl-mock:with-mocks ()
      (if-called 'http-request
                 (lambda (url &rest args)))
      (actually-send-webhook config "foo.bar" "{}"))))
