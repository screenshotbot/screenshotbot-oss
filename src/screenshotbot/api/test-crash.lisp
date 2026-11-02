;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-crash
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/crash
                #:%post-crash
                #:forward-crash)
  (:import-from #:cl-mock
                #:answer
                #:if-called
                #:with-mocks)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/api/test-crash)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-mocks ()
    (with-fake-request ()
      (let ((forwarded nil))
        (if-called 'sentry-client:forward-event
                   (lambda (event)
                     (push event forwarded)))
        (&body)))))

(test forwards-the-body-unchanged
  (with-fixture state ()
    (let ((event (flex:string-to-octets "{\"message\":\"é\"}" :external-format :utf-8)))
      (is (equal "{\"success\":true}" (forward-crash event)))
      (is (equalp (list event) forwarded)))))

(test empty-body-is-not-forwarded
  (with-fixture state ()
    (forward-crash nil)
    (is (eql hunchentoot:+http-bad-request+ (hunchentoot:return-code*)))
    (is (eql nil forwarded))))

(test handler-does-not-need-an-api-key
  (with-fixture state ()
    (answer (hunchentoot:raw-post-data :force-binary t) "{}")
    (%post-crash)
    (is (equal (list "{}") forwarded))))

(test works-without-a-configured-sentry-client
  (let ((sentry-client::*sentry-client* (make-instance 'sentry-client::noop-sentry-client)))
    (finishes
      (sentry-client:forward-event "{}"))))
