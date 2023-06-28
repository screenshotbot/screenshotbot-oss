;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-google-oauth
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/login/google-oauth
                #:google-domain
                #:google-oauth-provider)
  (:import-from #:oidc/oidc
                #:make-oidc-auth-link)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:auth
                #:with-sessions)
  (:import-from #:alexandria
                #:assoc-value))
(in-package :screenshotbot/login/test-google-oauth)

(util/fiveam:def-suite)

(test make-oidc-auth-link
  (let ((provider (make-instance 'google-oauth-provider
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
           (is (equal "example.com" (assoc-value params "hd" :test #'equal)))))))))


(test default-domain-is-nil
  "It's very likely we change the default domain for local testing, we
want to make sure we don't accidently push that change."
  (let ((provider (make-instance 'google-oauth-provider)))
    (is (null (google-domain provider)))))
