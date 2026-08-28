;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-device
  (:use #:cl
        #:fiveam)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:screenshotbot/auth-server/device
                #:%device-code)
  (:import-from #:screenshotbot/auth-server/errors
                #:with-oauth-json-errors)
  (:import-from #:screenshotbot/auth-server/model
                #:device-client
                #:device-scopes
                #:device-status
                #:find-device-request
                #:find-device-request-by-user-code
                #:normalize-user-code
                #:register-oauth-client)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:documentation "Tests for the device authorization endpoint, RFC 8628 §3.1-3.2."))
(in-package :screenshotbot/auth-server/test-device)

(util/fiveam:def-suite)

(defun decode-json (string)
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*identifier-name-to-key* #'identity))
    (json:decode-json-from-string string)))

(defun field (response name)
  (assoc-value response name :test #'equal))

(defun post-device-code (&rest params)
  (with-fake-request (:script-name "/oauth/device/code")
    (loop for (key value) on params by #'cddr
          if value
            do (setf (hunchentoot:post-parameter hunchentoot:*request* key) value))
    (decode-json
     (with-oauth-json-errors ()
       (%device-code)))))

(def-fixture state ()
  (with-test-store ()
    ;; No DECLARE in here: WITH-TEST-USER splices its body into a PROGN, so
    ;; a declaration is not in a declaration position. It already declares
    ;; its own bindings IGNORABLE anyway.
    (with-test-user (:company company :user user)
      (let ((client (register-oauth-client
                     :client-id "test-client"
                     :name "Test Client"
                     :scopes (list "profile" "api:read"))))
        (&body)))))

(test the-device-response-carries-everything-rfc-8628-requires
  (with-fixture state ()
    (let ((response (post-device-code "client_id" "test-client"
                                      "scope" "api:read")))
      (is-true (field response "device_code"))
      (is-true (field response "user_code"))
      (is-true (field response "verification_uri"))
      (is-true (field response "verification_uri_complete"))
      (is (< 0 (field response "expires_in")))
      (is (equal 5 (field response "interval"))))))

(test the-device-code-and-user-code-are-different-secrets
  "The user code is meant to be read aloud; the device code is the actual
credential and must not be derivable from it."
  (with-fixture state ()
    (let ((response (post-device-code "client_id" "test-client")))
      (is-false (equal (field response "device_code")
                       (field response "user_code")))
      (is (>= (length (field response "device_code")) 40))
      (is (equal 9 (length (field response "user_code")))))))

(test the-complete-uri-carries-the-user-code-url-encoded
  (with-fixture state ()
    (let* ((response (post-device-code "client_id" "test-client"))
           (complete (field response "verification_uri_complete")))
      (is-true (str:containsp "user_code=" complete))
      (is-true (str:starts-with-p (field response "verification_uri") complete))
      (is (equal (field response "user_code")
                 (quri:url-decode
                  (second (str:split "user_code=" complete :limit 2))))))))

(test the-request-is-pending-and-findable-by-either-code
  (with-fixture state ()
    (let* ((response (post-device-code "client_id" "test-client" "scope" "api:read"))
           (by-device (find-device-request (field response "device_code")))
           (by-user (find-device-request-by-user-code
                     (normalize-user-code (field response "user_code")))))
      (is-true by-device)
      (is (eq by-device by-user))
      (is (eql :pending (device-status by-device)))
      (is (eq client (device-client by-device)))
      (is (equal '("api:read") (device-scopes by-device))))))

(test each-request-gets-its-own-codes
  (with-fixture state ()
    (let ((first (post-device-code "client_id" "test-client"))
          (second (post-device-code "client_id" "test-client")))
      (is-false (equal (field first "device_code") (field second "device_code")))
      (is-false (equal (field first "user_code") (field second "user_code"))))))

(test an-unknown-client-cannot-start-a-device-flow
  (with-fixture state ()
    (is (equal "invalid_client"
               (field (post-device-code "client_id" "nope") "error")))
    (is (equal "invalid_client"
               (field (post-device-code) "error")))))

(test an-unsupported-scope-is-refused-by-name
  (with-fixture state ()
    (let ((response (post-device-code "client_id" "test-client"
                                      "scope" "api:read admin:everything")))
      (is (equal "invalid_scope" (field response "error")))
      (is-true (str:containsp "admin:everything" (field response "error_description"))))))

(test a-scope-the-client-may-not-have-is-refused
  (with-fixture state ()
    ;; The fixture's client is allowed profile and api:read, not api:write.
    (is (equal "invalid_scope"
               (field (post-device-code "client_id" "test-client"
                                        "scope" "api:write")
                      "error")))))

(test asking-for-no-scope-grants-the-least-privileged-set
  (with-fixture state ()
    (let* ((response (post-device-code "client_id" "test-client"))
           (request (find-device-request (field response "device_code"))))
      (is (equal '("profile") (device-scopes request))))))
