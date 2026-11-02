;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-crash
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/crash
                #:*throttler*
                #:%post-crash
                #:forward-crash)
  (:import-from #:cl-mock
                #:answer
                #:if-called
                #:with-mocks)
  (:import-from #:core/api/model/api-key
                #:api-key
                #:encode-api-token)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:util/throttler
                #:ip-throttler))
(in-package :screenshotbot/api/test-crash)


(util/fiveam:def-suite)

(defclass my-installation (multi-org-feature
                           installation)
  ())

(def-fixture state ()
  (with-installation (:installation (make-instance 'my-installation))
    (with-test-store ()
      (with-test-user (:user user :company company)
        (with-mocks ()
          (with-fake-request ()
            (let ((forwarded nil)
                  (*throttler* (make-instance 'ip-throttler :tokens 1)))
              (if-called 'sentry-client:forward-event
                         (lambda (event)
                           (push event forwarded)))
              (answer (hunchentoot:raw-post-data :force-binary t) "{}")
              (flet ((use-credentials (key secret)
                       (answer (hunchentoot:header-in* :authorization) "Basic foo")
                       (answer (hunchentoot:authorization) (values key secret))))
                (declare (ignorable #'use-credentials))
                (&body)))))))))

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

(test without-credentials-is-throttled
  (with-fixture state ()
    (%post-crash)
    (is (equal (list "{}") forwarded))
    (is (not (eql 429 (hunchentoot:return-code*))))
    (%post-crash)
    (is (eql 429 (hunchentoot:return-code*)))
    (is (equal (list "{}") forwarded))))

(test invalid-credentials-is-throttled
  (with-fixture state ()
    (use-credentials "no-such-key" "bad-secret")
    (%post-crash)
    (is (equal (list "{}") forwarded))
    (%post-crash)
    (is (eql 429 (hunchentoot:return-code*)))
    (is (equal (list "{}") forwarded))))

(test valid-credentials-is-not-throttled
  (with-fixture state ()
    (let ((key (make-instance 'api-key :user user :company company)))
      (use-credentials "" (encode-api-token key))
      (dotimes (i 3)
        (%post-crash))
      (is (not (eql 429 (hunchentoot:return-code*))))
      (is (equal (list "{}" "{}" "{}") forwarded)))))

(test works-without-a-configured-sentry-client
  (let ((sentry-client::*sentry-client* (make-instance 'sentry-client::noop-sentry-client)))
    (finishes
      (sentry-client:forward-event "{}"))))
