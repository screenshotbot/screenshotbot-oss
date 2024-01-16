;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-google-oauth
  (:use :cl)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:auth
                #:with-sessions)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:finishes
                #:is
                #:signals
                #:test
                #:with-fixture)
  (:import-from #:oidc/oidc
                #:discover
                #:make-oidc-auth-link
                #:verify-userinfo)
  (:import-from #:screenshotbot/login/google-oauth
                #:google-domain
                #:google-oauth-provider)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/login/test-google-oauth)

(util/fiveam:def-suite)

(def-fixture state ()
  (&body))


(defclass test-google-oauth-provider (google-oauth-provider)
  ())

(defmethod discover ((self test-google-oauth-provider))
  `((:ISSUER . "https://accounts.google.com") (:AUTHORIZATION--ENDPOINT . "https://accounts.google.com/o/oauth2/v2/auth") (:DEVICE--AUTHORIZATION--ENDPOINT . "https://oauth2.googleapis.com/device/code") (:TOKEN--ENDPOINT . "https://oauth2.googleapis.com/token") (:USERINFO--ENDPOINT . "https://openidconnect.googleapis.com/v1/userinfo") (:REVOCATION--ENDPOINT . "https://oauth2.googleapis.com/revoke") (:JWKS--URI . "https://www.googleapis.com/oauth2/v3/certs") (:RESPONSE--TYPES--SUPPORTED "code" "token" "id_token" "code token" "code id_token" "token id_token" "code token id_token" "none") (:SUBJECT--TYPES--SUPPORTED "public") (:ID--TOKEN--SIGNING--ALG--VALUES--SUPPORTED "RS256") (:SCOPES--SUPPORTED "openid" "email" "profile") (:TOKEN--ENDPOINT--AUTH--METHODS--SUPPORTED "client_secret_post" "client_secret_basic") (:CLAIMS--SUPPORTED "aud" "email" "email_verified" "exp" "family_name" "given_name" "iat" "iss" "locale" "name" "picture" "sub") (:CODE--CHALLENGE--METHODS--SUPPORTED "plain" "S256") (:GRANT--TYPES--SUPPORTED "authorization_code" "refresh_token" "urn:ietf:params:oauth:grant-type:device_code" "urn:ietf:params:oauth:grant-type:jwt-bearer")))

(test make-oidc-auth-link-for-google
  (with-fixture state ()
    (let ((provider (make-instance 'test-google-oauth-provider
                                   :client-id "foobar"
                                   :client-secret "barbar"
                                   :domain "example.com")))
      (with-fake-request ()
        (with-sessions ()
          (let ((uri (make-oidc-auth-link provider "https://foobar.com")))
            (is (stringp uri))
            (let* ((uri (quri:uri uri))
                   (params (quri:uri-query-params uri)))
              (is (equal "foobar" (assoc-value params "client_id" :test #'equal)))
              (is (equal "example.com" (assoc-value params "hd" :test #'equal))))))))))


(test default-domain-is-nil
  "It's very likely we change the default domain for local testing, we
want to make sure we don't accidently push that change."
  (with-fixture state ()
   (let ((provider (make-instance 'google-oauth-provider)))
     (is (null (google-domain provider))))))

(test verify-userinfo-for-google
  (with-fixture state ()
   (let ((provider (make-instance 'google-oauth-provider
                                  :domain nil)))
     (finishes
       (verify-userinfo
        provider nil)))))

(test verify-userinfo-for-google-with-domain
  (with-fixture state ()
   (let ((provider (make-instance 'google-oauth-provider
                                  :domain "example.com")))
     (finishes
       (verify-userinfo
        provider `((:hd . "example.com"))))
     (signals simple-error
       (verify-userinfo
        provider `((:hd . "foo.com")))))))
