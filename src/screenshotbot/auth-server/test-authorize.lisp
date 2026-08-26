;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-authorize
  (:use #:cl
        #:fiveam)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:cl-mock
                #:if-called
                #:with-mocks)
  (:import-from #:screenshotbot/auth-server/authorize
                #:%authorize
                #:%error-page
                #:%grant-and-redirect
                #:consent-page)
  (:import-from #:screenshotbot/auth-server/model
                #:code-challenge
                #:code-challenge-method
                #:code-grant
                #:code-redirect-uri
                #:find-oauth-code
                #:grant-client
                #:grant-scopes
                #:grant-user
                #:register-oauth-client)
  (:import-from #:screenshotbot/auth-server/pkce
                #:random-token
                #:s256-challenge)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/auth-server/test-authorize)

(util/fiveam:def-suite)

(defparameter +redirect-uri+ "http://127.0.0.1:43219/callback")

(defun capture-redirect (fn)
  "Run FN and return the URL it tried to redirect to, or NIL.

HEX:SAFE-REDIRECT signals HEX:REDIRECTED before it unwinds the request,
so we can grab the target without an acceptor underneath us."
  (block nil
    (handler-bind ((hex:redirected
                     (lambda (c)
                       (return (slot-value c 'hex::url)))))
      (funcall fn)
      nil)))

(defun query-param (url name)
  (assoc-value (quri:uri-query-params (quri:uri url)) name :test #'equal))

(def-fixture state ()
  (with-mocks ()
    (with-test-store ()
      (with-test-user (:company company :user user :logged-in-p t)
        (let* ((verifier (random-token 32))
               (challenge (s256-challenge verifier))
               (consented nil)
               (errored nil)
               (client (register-oauth-client
                        :client-id "test-client"
                        :name "Test Client"
                        :redirect-uris (list "http://127.0.0.1/callback")
                        :scopes (list "profile" "api:read"))))
          (declare (ignorable client))
          ;; Both terminal renderings are stubbed: this file is about which
          ;; branch we take, not about markup.
          (if-called 'consent-page
                     (lambda (&rest args)
                       (setf consented args)
                       :consent))
          (if-called '%error-page
                     (lambda (title message)
                       (setf errored (list title message))
                       :error-page))
          (flet ((authorize (&rest args)
                   ;; ARGS first: for duplicate keywords the leftmost
                   ;; wins, so this is what lets a test override one
                   ;; parameter of an otherwise valid request.
                   (capture-redirect
                    (lambda ()
                      (apply #'%authorize
                             (append
                              args
                              (list :response-type "code"
                                    :client-id "test-client"
                                    :redirect-uri +redirect-uri+
                                    :scope "api:read"
                                    :state "opaque-state"
                                    :code-challenge challenge
                                    :code-challenge-method "S256")))))))
            (&body)))))))

;; ----------------------------------------------------------------------
;; Requests we must not redirect
;; ----------------------------------------------------------------------

(test a-valid-request-reaches-the-consent-screen
  (with-fixture state ()
    (is-false (authorize))
    (is-true consented)
    (is (equal '("api:read") (getf consented :scopes)))
    (is (equal +redirect-uri+ (getf consented :redirect-uri)))
    (is (equal "opaque-state" (getf consented :state)))
    (is (equal challenge (getf consented :challenge)))
    (is (equal "S256" (getf consented :challenge-method)))
    (is-false errored)))

(test an-unknown-client-is-shown-an-error-not-a-redirect
  "RFC 6749 §4.1.2.1: with no trustworthy client we have no trustworthy
redirect target either."
  (with-fixture state ()
    (is-false (authorize :client-id "no-such-client"))
    (is-true errored)
    (is-false consented)))

(test a-missing-client-id-is-shown-an-error
  (with-fixture state ()
    (is-false (authorize :client-id nil))
    (is-true errored)))

(test an-unregistered-redirect-uri-is-never-redirected-to
  "The whole attack this guards against is us bouncing the user, and later
their code, to a URL the client never registered."
  (with-fixture state ()
    (is-false (authorize :redirect-uri "https://evil.example.com/steal"))
    (is-true errored)
    (is-false consented)))

(test a-missing-redirect-uri-is-shown-an-error
  (with-fixture state ()
    (is-false (authorize :redirect-uri nil))
    (is-true errored)))

(test a-loopback-redirect-on-any-port-is-accepted
  (with-fixture state ()
    (is-false (authorize :redirect-uri "http://127.0.0.1:1/callback"))
    (is-true consented)
    (is-false errored)))

;; ----------------------------------------------------------------------
;; Errors that go back to the client
;; ----------------------------------------------------------------------

(test an-unsupported-response-type-redirects-with-the-state-intact
  (with-fixture state ()
    (let ((url (authorize :response-type "token")))
      (is (equal "unsupported_response_type" (query-param url "error")))
      (is (equal "opaque-state" (query-param url "state")))
      (is-false (query-param url "code"))
      (is (str:starts-with-p "http://127.0.0.1:43219/callback" url)))
    (is-false consented)))

(test an-unknown-scope-redirects-with-invalid-scope
  (with-fixture state ()
    (let ((url (authorize :scope "api:read admin:everything")))
      (is (equal "invalid_scope" (query-param url "error")))
      (is (str:containsp "admin:everything" (query-param url "error_description"))))))

(test a-scope-the-client-may-not-have-redirects-with-invalid-scope
  (with-fixture state ()
    ;; The fixture's client is only allowed profile and api:read.
    (let ((url (authorize :scope "api:write")))
      (is (equal "invalid_scope" (query-param url "error"))))))

(test a-public-client-must-send-a-code-challenge
  (with-fixture state ()
    (let ((url (authorize :code-challenge nil)))
      (is (equal "invalid_request" (query-param url "error")))
      (is (str:containsp "code_challenge" (query-param url "error_description"))))))

(test plain-pkce-is-refused
  (with-fixture state ()
    (is (equal "invalid_request"
               (query-param (authorize :code-challenge-method "plain") "error")))
    ;; A missing method defaults to plain per RFC 7636 §4.3, so it is
    ;; refused for the same reason.
    (is (equal "invalid_request"
               (query-param (authorize :code-challenge-method nil) "error")))))

(test errors-preserve-a-query-the-client-already-had
  (with-fixture state ()
    (let* ((c (register-oauth-client
               :client-id "query-client"
               :redirect-uris (list "https://app.example.com/cb?tenant=acme")))
           (url (capture-redirect
                 (lambda ()
                   (%authorize :response-type "token"
                               :client-id "query-client"
                               :redirect-uri "https://app.example.com/cb?tenant=acme"
                               :state "s")))))
      (declare (ignore c))
      (is (equal "acme" (query-param url "tenant")))
      (is (equal "unsupported_response_type" (query-param url "error"))))))

(test a-request-with-no-state-does-not-emit-an-empty-state
  (with-fixture state ()
    (let ((url (authorize :response-type "token" :state nil)))
      (is-false (query-param url "state")))))

;; ----------------------------------------------------------------------
;; Granting
;; ----------------------------------------------------------------------

(test approving-issues-a-code-bound-to-the-request
  (with-fixture state ()
    (let ((url (capture-redirect
                (lambda ()
                  (%grant-and-redirect :client client
                                       :redirect-uri +redirect-uri+
                                       :scopes '("api:read")
                                       :state "opaque-state"
                                       :challenge challenge
                                       :challenge-method "S256")))))
      (is (equal "opaque-state" (query-param url "state")))
      (is (str:starts-with-p "http://127.0.0.1:43219/callback" url))
      (let ((code (find-oauth-code (query-param url "code"))))
        (is-true code)
        (is (equal +redirect-uri+ (code-redirect-uri code)))
        (is (equal challenge (code-challenge code)))
        (is (equal "S256" (code-challenge-method code)))
        (let ((grant (code-grant code)))
          (is (eq client (grant-client grant)))
          (is (eq user (grant-user grant)))
          (is (equal '("api:read") (grant-scopes grant))))))))
