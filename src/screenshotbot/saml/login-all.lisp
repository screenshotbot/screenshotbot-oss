;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/saml/login-all
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/login/saml
                #:saml-auth-providers-for-company
                #:saml-name)
  (:import-from #:screenshotbot/default-oidc-provider
                #:default-oidc-provider
                #:oidc-providers-for-company
                #:make-oidc-provider
                #:issuer)
  (:import-from #:screenshotbot/login/common
                #:auth-template
                #:auth-common-header)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/model/company
                #:get-singleton-company)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:oidc/oidc
                #:make-oidc-auth-link)
  (:import-from #:util/store/object-id
                #:find-by-oid
                #:oid)
  (:export
   #:login-all-url))
(in-package :screenshotbot/saml/login-all)

(named-readtables:in-readtable markup:syntax)

(defun login-all-url ()
  "/account/login/all")

(defun not-found ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (hunchentoot:abort-request-handler))

(defun login-company ()
  "The company whose SSO connections can be used to sign in, or NIL.
Only single-company installations, since the page is public: on a
multi-org installation it would list every company's IdPs."
  (unless (typep *installation* 'multi-org-feature)
    (let ((company (get-singleton-company *installation*)))
      (when (gk:check :self-service-saml company)
        company))))

(defun login-with-provider-url (provider redirect)
  (let ((path (typecase provider
                (default-oidc-provider "/oidc/~a/login")
                (t "/saml/~a/login"))))
    (quri:render-uri
     (quri:make-uri :path (format nil path (oid provider))
                    :query (when redirect
                             `(("redirect" . ,redirect)))))))

(defhandler (oidc-login :uri "/oidc/:oid/login") (oid redirect)
  "Sign in with any OIDC connection, not just the default one. The OIDC
callback is a nibble, so the link is only made when someone clicks it."
  (let ((company (login-company))
        (oidc (find-by-oid oid)))
    (unless (and company
                 (typep oidc 'default-oidc-provider)
                 (find oidc (fset:convert 'list (oidc-providers-for-company company))))
      (not-found))
    (hex:safe-redirect (make-oidc-auth-link (make-oidc-provider oidc)
                                            (or redirect "/runs")))))

(markup:deftag provider-button (&key href name)
  <div class= "form-group mt-1 text-center mb-2">
    <a class= "btn btn-outline-secondary" style= "width:100%" href=href >
      <span class= "ms-1">Sign in with ,(progn name)</span>
    </a>
  </div>)

(defhandler (login-all-page :uri "/account/login/all") (redirect)
  "Every SSO connection that can be used to sign in, including the ones
that aren't the default."
  (let ((company (login-company)))
    (unless company
      (not-found))
    (let ((samls (fset:convert 'list (saml-auth-providers-for-company company)))
          (oidcs (fset:convert 'list (oidc-providers-for-company company))))
      <auth-template body-class= "signin-v2" simple=t >
        <div class= "account-pages mt-5 mb-5">
          <div class= "card border-0 account-card">
            <div class= "card-header">
              <auth-common-header>
                Sign in with single sign-on
              </auth-common-header>
            </div>
            <div class= "card-body p-4">
              ,@(loop for saml in samls
                      collect <provider-button href= (login-with-provider-url saml redirect)
                                               name= (saml-name saml) />)
              ,@(loop for oidc in oidcs
                      collect <provider-button href= (login-with-provider-url oidc redirect)
                                               name= (issuer oidc) />)
              ,(unless (or samls oidcs)
                 <p class= "text-muted text-center">No single sign-on connections are configured.</p>)
            </div>
          </div>
          <div class= "row mt-3">
            <div class= "col-12 text-center">
              <p><a href= "/signin">Back to sign in</a></p>
            </div>
          </div>
        </div>
      </auth-template>)))
