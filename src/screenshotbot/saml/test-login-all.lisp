;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/saml/test-login-all
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/saml/login-all
                #:login-all-page
                #:oidc-login
                #:login-with-provider-url)
  (:import-from #:screenshotbot/login/saml
                #:saml-auth-provider)
  (:import-from #:screenshotbot/default-oidc-provider
                #:default-oidc-provider
                #:stored-oidc-provider
                #:issuer)
  (:import-from #:oidc/oidc
                #:make-oidc-auth-link)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/model/company
                #:company
                #:get-singleton-company)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:util/testing
                #:screenshot-static-page
                #:with-fake-request)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/store/object-id
                #:oid))
(in-package :screenshotbot/saml/test-login-all)

(util/fiveam:def-suite)

(defclass multi-org-installation (multi-org-feature
                                  screenshotbot/installation:installation)
  ())

(def-fixture state (&key (enabled t) multi-org)
  (with-test-store ()
    (let ((*installation* (if multi-org
                              (make-instance 'multi-org-installation)
                              (make-instance 'screenshotbot/installation:installation))))
      (with-fake-request ()
        (auth:with-sessions ()
          (gk:create :self-service-saml)
          (let ((company (unless multi-org
                           (get-singleton-company *installation*))))
            (when (and company enabled)
              (gk:allow :self-service-saml company))
            (&body)))))))

(defun make-saml (company &key (name "Okta") defaultp)
  (make-instance 'saml-auth-provider
                 :company company
                 :name name
                 :defaultp defaultp))

(defun make-oidc (company &key (issuer "https://idp.example.com"))
  (make-instance 'default-oidc-provider
                 :company company
                 :issuer issuer
                 :client-id "client-id"
                 :client-secret "very-secret"
                 :scope "openid email profile"))

(defun render ()
  (markup:write-html (login-all-page)))

(defun not-found-p (fn)
  (catch 'hunchentoot::handler-done
    (funcall fn)
    (return-from not-found-p nil))
  (eql hunchentoot:+http-not-found+ (hunchentoot:return-code*)))

(test lists-every-provider
  (with-fixture state ()
    (let ((saml (make-saml company :name "Okta" :defaultp t))
          (other-saml (make-saml company :name "Entra"))
          (oidc (make-oidc company :issuer "https://auth.example.com")))
      (let ((html (render)))
        (is (str:containsp "Sign in with Okta" html))
        (is (str:containsp "Sign in with Entra" html))
        (is (str:containsp "Sign in with https://auth.example.com" html))
        (is (str:containsp (format nil "/saml/~a/login" (oid saml)) html))
        (is (str:containsp (format nil "/saml/~a/login" (oid other-saml)) html))
        (is (str:containsp (format nil "/oidc/~a/login" (oid oidc)) html))
        (is (not (str:containsp "very-secret" html)))))))

(test doesnt-list-another-companys-providers
  (with-fixture state ()
    (make-saml (make-instance 'company) :name "Elsewhere")
    (is (not (str:containsp "Elsewhere" (render))))))

(test says-when-there-are-no-providers
  (with-fixture state ()
    (is (str:containsp "No single sign-on connections are configured" (render)))))

(test links-keep-the-redirect
  (with-fixture state ()
    (let ((oidc (make-oidc company)))
      (is (equal (format nil "/oidc/~a/login?redirect=%2Fchannels" (oid oidc))
                 (login-with-provider-url oidc "/channels")))
      (is (equal (format nil "/oidc/~a/login" (oid oidc))
                 (login-with-provider-url oidc nil))))))

(test needs-the-gatekeeper
  (with-fixture state (:enabled nil)
    (is-true (not-found-p #'login-all-page))))

(test not-on-multi-org-installations
  (with-fixture state (:multi-org t)
    (is-true (not-found-p #'login-all-page))))

(test oidc-login-uses-a-stored-oidc-provider
  (with-fixture state ()
    (let ((oidc (make-oidc company :issuer "https://auth.example.com"))
          (seen nil))
      (cl-mock:with-mocks ()
        (cl-mock:if-called 'make-oidc-auth-link
                           (lambda (auth redirect &key &allow-other-keys)
                             (setf seen (list auth redirect))
                             "https://auth.example.com/authorize"))
        (handler-case
            (progn
              (oidc-login :oid (oid oidc) :redirect "/channels")
              (fail "Expected a redirect"))
          (hex:redirected (e)
            (is (equal "https://auth.example.com/authorize"
                       (slot-value e 'hex::url))))))
      (destructuring-bind (auth redirect) seen
        ;; Not a test login, and it still refuses site admins
        (is (eql 'stored-oidc-provider (type-of auth)))
        (is (equal "https://auth.example.com" (oidc/oidc:issuer auth)))
        (is (equal "/channels" redirect))))))

(test oidc-login-for-another-company-is-not-found
  (with-fixture state ()
    (let ((oidc (make-oidc (make-instance 'company))))
      (is-true (not-found-p (lambda () (oidc-login :oid (oid oidc))))))))

(test oidc-login-for-a-saml-provider-is-not-found
  (with-fixture state ()
    (let ((saml (make-saml company)))
      (is-true (not-found-p (lambda () (oidc-login :oid (oid saml))))))))

(test screenshot
  (with-fixture state ()
    (make-saml company :name "Okta" :defaultp t)
    (make-saml company :name "Microsoft Entra ID")
    (make-oidc company :issuer "https://auth.example.com")
    (screenshot-static-page
     :screenshotbot
     "login-all"
     (render))))

(test screenshot-without-providers
  (with-fixture state ()
    (screenshot-static-page
     :screenshotbot
     "login-all-without-providers"
     (render))))
