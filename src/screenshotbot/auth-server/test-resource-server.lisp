;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-resource-server
  (:use #:cl
        #:fiveam)
  (:import-from #:cl-mock
                #:if-called
                #:with-mocks)
  (:import-from #:screenshotbot/api/core
                #:api-error
                #:authenticate-api-request
                #:bearer-token)
  (:import-from #:screenshotbot/auth-server/resource-server
                #:bearer-challenge
                #:send-unauthorized
                #:with-bearer-authentication)
  (:import-from #:util/testing
                #:with-fake-request)
  (:documentation "Tests for the 401 challenge that MCP clients rely on to
find the authorization server."))
(in-package :screenshotbot/auth-server/test-resource-server)

(util/fiveam:def-suite)

(defparameter +metadata-url+
  "https://staging.screenshotbot.io/.well-known/oauth-protected-resource/mcp")

;; ----------------------------------------------------------------------
;; The challenge itself
;; ----------------------------------------------------------------------

(test a-bare-challenge-names-the-metadata-document
  "This parameter is the whole point: without it a client that has only
seen /mcp has no way to discover where to authenticate."
  (is (equal (format nil "Bearer resource_metadata=~s" +metadata-url+)
             (bearer-challenge +metadata-url+))))

(test a-request-with-no-credentials-gets-no-error-code
  "RFC 6750 §3.1: there was nothing wrong with the token, there just
wasn't one."
  (let ((challenge (bearer-challenge +metadata-url+)))
    (is-false (str:containsp "error=" challenge))
    (is-false (str:containsp "error_description=" challenge))))

(test a-rejected-token-is-described-in-the-challenge
  (let ((challenge (bearer-challenge +metadata-url+
                                     :error "invalid_token"
                                     :description "expired")))
    (is-true (str:containsp "error=\"invalid_token\"" challenge))
    (is-true (str:containsp "error_description=\"expired\"" challenge))
    (is-true (str:starts-with-p "Bearer resource_metadata=" challenge))))

(test the-challenge-cannot-be-broken-out-of
  "A quote or a newline in a header value would let the rest of the
response be forged."
  (let ((challenge (bearer-challenge +metadata-url+
                                     :error "invalid_token"
                                     :description
                                     (format nil "ab\"cd\\ef~C~Cgh" #\Return #\Newline))))
    (is (equal "abcdefgh"
               (str:substring 0 8
                              (second (str:split "error_description=\"" challenge)))))
    (is-false (find #\Return challenge))
    (is-false (find #\Newline challenge))
    ;; Exactly the quotes we opened, none smuggled in.
    (is (equal 6 (count #\" challenge)))))

;; ----------------------------------------------------------------------
;; The response
;; ----------------------------------------------------------------------

(test an-unauthorized-response-is-a-401-carrying-the-challenge
  (with-fake-request ()
    (let ((body (send-unauthorized +metadata-url+)))
      (is (equal 401 (hunchentoot:return-code*)))
      (is (equal (bearer-challenge +metadata-url+)
                 (hunchentoot:header-out :www-authenticate)))
      (is (equal "no-store" (hunchentoot:header-out :cache-control)))
      ;; Readable by something expecting JSON-RPC, with a null id because
      ;; we reject before ever parsing the request.
      (is-true (str:containsp "\"jsonrpc\":\"2.0\"" body))
      (is-true (str:containsp "\"id\":null" body)))))

;; ----------------------------------------------------------------------
;; The wrapper
;; ----------------------------------------------------------------------

(def-fixture state ()
  (with-mocks ()
    (with-fake-request ()
      (let ((ran nil))
        (flet ((protected ()
                 (with-bearer-authentication (:resource-metadata-url +metadata-url+)
                   (setf ran t)
                   "the-body")))
          (&body))))))

(test a-valid-token-runs-the-body
  (with-fixture state ()
    (if-called 'bearer-token (lambda () "a-token"))
    (if-called 'authenticate-api-request (lambda (request)
                                           (declare (ignore request))
                                           :the-api-key))
    (is (equal "the-body" (protected)))
    (is-true ran)))

(test a-request-with-no-token-is-challenged-without-an-error-code
  (with-fixture state ()
    (if-called 'bearer-token (lambda () nil))
    (if-called 'authenticate-api-request
               (lambda (request)
                 (declare (ignore request))
                 (error 'api-error :message "No such API key: NIL")))
    (protected)
    (is-false ran)
    (is (equal 401 (hunchentoot:return-code*)))
    (let ((challenge (hunchentoot:header-out :www-authenticate)))
      (is-true (str:containsp "resource_metadata=" challenge))
      (is-false (str:containsp "error=" challenge)))))

(test a-request-with-a-bad-token-is-told-the-token-was-the-problem
  (with-fixture state ()
    (if-called 'bearer-token (lambda () "a-bad-token"))
    (if-called 'authenticate-api-request
               (lambda (request)
                 (declare (ignore request))
                 (error 'api-error :message "API secret key doesn't match")))
    (protected)
    (is-false ran)
    (is (equal 401 (hunchentoot:return-code*)))
    (let ((challenge (hunchentoot:header-out :www-authenticate)))
      (is-true (str:containsp "error=\"invalid_token\"" challenge))
      (is-true (str:containsp "doesn't match" challenge)))))

(test an-unexpected-failure-does-not-leak-its-message
  "An API-ERROR is written for callers; anything else could be an internal
detail, and a 401 body is not the place to find out."
  (with-fixture state ()
    (if-called 'bearer-token (lambda () "a-token"))
    (if-called 'authenticate-api-request
               (lambda (request)
                 (declare (ignore request))
                 (error "database on fire at /var/lib/secret")))
    (protected)
    (is-false ran)
    (is (equal 401 (hunchentoot:return-code*)))
    (let ((challenge (hunchentoot:header-out :www-authenticate)))
      (is-true (str:containsp "error=\"invalid_token\"" challenge))
      (is-false (str:containsp "secret" challenge))
      (is-false (str:containsp "fire" challenge)))))

(test the-body-is-never-reached-when-authentication-fails
  "The whole point: /mcp was previously unauthenticated."
  (with-fixture state ()
    (if-called 'bearer-token (lambda () nil))
    (if-called 'authenticate-api-request
               (lambda (request)
                 (declare (ignore request))
                 (error 'api-error :message "nope")))
    (let ((body (protected)))
      (is-false ran)
      (is-false (equal "the-body" body)))))
