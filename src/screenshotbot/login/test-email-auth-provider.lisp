;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-email-auth-provider
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/login/email-auth-provider
                #:make-login-url
                #:screenshotbot-email-auth-provider)
  (:import-from #:screenshotbot/model/enterprise
                #:enterprise-install)
  (:import-from #:screenshotbot/login/common
                #:email-redirect-url))
(in-package :screenshotbot/login/test-email-auth-provider)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((e1 (make-instance 'enterprise-install
                             :domain "https://acme.screenshotbot.io"
                             :email-domains (list "acme.com")))
          (e2 (make-instance 'enterprise-install
                             :domain "https://example.screenshotbot.io"
                             :email-domains (list "foo.com" "example.com")))
          (self (make-instance 'screenshotbot-email-auth-provider)))
      (&body))))

(test email-redirect-url-for-non-existant-domain
  (with-fixture state ()
    (is (eql nil (email-redirect-url self "gmail.com")))))

(test redirects-correctly
  (with-fixture state ()
    (is (equal "https://acme.screenshotbot.io/login"
               (email-redirect-url self "acme.com")))))

(test make-login-url
  (is (equal "https://acme.screenshotbot.io/login"
             (make-login-url "https://acme.screenshotbot.io"))))

(test second-domain
  (with-fixture state ()
    (is (equal "https://example.screenshotbot.io/login"
               (email-redirect-url self "foo.com")))))


