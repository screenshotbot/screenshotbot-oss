;;;; -*- coding: utf-8 -*-
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/login
  (:use :cl)
  (:nicknames :auth/login/login)
  (:import-from #:alexandria
                #:if-let)
  (:import-from #:auth
                #:current-user
                #:logged-in-p)
  (:import-from #:core/installation/auth-provider
                #:auth-provider-signin-form
                #:auth-providers
                #:default-oidc-provider)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:markup/markup
                #:deftag
                #:unescaped)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:oidc/oidc
                #:end-session-endpoint)
  (:import-from #:screenshotbot/impersonation
                #:impersonation
                #:make-impersonation)
  (:import-from #:screenshotbot/login/common
                #:auth-template
                #:ip-throttler
                #:oauth-signin-link
                #:signin-get
                #:signup-get
                #:standard-auth-provider)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:util/throttler
                #:throttle!)
  (:export
   #:auth-header-logo))
(in-package :screenshotbot/login/login)

(markup:enable-reader)

(defvar *throttler* (make-instance 'ip-throttler
                                   :tokens 1200))

(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/login") ()
  (hex:safe-redirect "/signin"))


(defmethod auth-provider-signin-form ((auth-provider standard-auth-provider) redirect)

  (let ((result (nibble (email password)
                  (signin-post :email email
                               :password password
                               :redirect redirect))))
  <form action=result method= "POST" >
    <div class="form-group mb-3">
      <input name= "email"  class="form-control" type="email" id="emailaddress" required="" placeholder="Email" />
    </div>

    <div class="form-group mb-3">
      <div class="input-group input-group-merge">
        <input name= "password" type="password" id="password" class="form-control" placeholder="Password" />
      </div>
    </div>

    <div class= "d-flex justify-content-between">
      <div class="form-check ps-0">
        <input type="checkbox" class="form-check-input me-3" id="checkbox-signin" checked= "checked" />
        <label class="form-check-label" for="checkbox-signin">Remember me</label>
      </div>

      <div>
        <a href="/forgot-password" class="links"><small>Forgot your password?</small></a>
      </div>

    </div>

    <div class= "float-end">

    </div>


    <div class="form-group mb-0">
      <button class="btn btn-primary" type="submit"> Sign In </button>
    </div>
  </form>))

(defmethod default-login-redirect (request)
  "/runs")

(deftag signin-get (&key (redirect (default-login-redirect hunchentoot:*request*)) (alert nil))
  (assert redirect)
  (if-let ((provider (default-oidc-provider *installation*)))
    (hex:safe-redirect (oauth-signin-link provider redirect))
    (let ((signup (nibble ()
                    (signup-get :redirect redirect
                                :alert alert))))
      <auth-template>

        <div class="account-pages mt-5 mb-5">
          ,(progn alert)
          <div class="card">

            <div class="">
              <h4 class="text-dark-50 ps-4 mt-0 font-weight-bold">Sign In</h4>
            </div>

            <div class="card-body p-4">
              ,@ (loop for auth-provider in (auth-providers *installation*)
                       collect
                       (auth-provider-signin-form auth-provider redirect))
            </div> <!-- end card-body -->
          </div>
          <!-- end card -->

          <div class="row mt-3">
            <div class="col-12 text-center">
              <p class="signup-message">Don't have an account? <a href=signup class="ms-1"><b>Sign Up</b></a></p>

            </div> <!-- end col -->
          </div>
          <!-- end row -->
        </div>
        <!-- end page -->
      </auth-template>)))

(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/signin") (after-logout redirect)
  (declare (ignore after-logout))
  (let ((redirect (or redirect (default-login-redirect hunchentoot:*request*))))
    (when (logged-in-p)
      (hex:safe-redirect redirect))

    (signin-get :redirect redirect)))

(defun signin-post (&key email password redirect)
  (throttle! *throttler*)
  (let (errors
        (user (auth:find-user *installation* :email email)))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check :email (not (str:emptyp email))
             "Please enter an email")
      (check :password (not (str:emptyp password))
             "Please enter a password")
      (check :password (< (length password) 150)
             "Password too long")
      (check :email user
             "No account associated with that email address")
      (when user
        (check :password (auth:password-hash user)
               "This account appears to use an OAuth (either Google or GitHub)")

        (when (auth:password-hash user)
          (check :password (auth:check-password user password)
                 "Password does not match the given account"))))
    (cond
      (errors
       (with-form-errors (:email email :errors errors
                            :was-validated t)
        (signin-get)))
      (t
       (assert (auth:check-password user password))
       (setf (current-user) user)
       (hex:safe-redirect redirect)))))


(defun hard-signout ()
  (setf (current-user) nil)
  (hex:safe-redirect "/"))

(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/account/oauth-signout-callback") ()
  (hard-signout))

(defun signout-from-oidc (auth-provider)
  (let ((endpoint (end-session-endpoint auth-provider)))
    (cond
      (endpoint
       (hunchentoot:redirect
        (quri:render-uri
         (quri:copy-uri
          (quri:uri endpoint)
          :query `(("post_logout_redirect_uri" . ,(hex:make-full-url hunchentoot:*request* "/account/oauth-signout-callback")))))))
      (t
       (hard-signout)))))

(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/signout") ()
  (let ((impersonation (make-impersonation)))
    (screenshotbot/impersonation:logout impersonation)
    (if-let ((default-auth-provider (default-oidc-provider *installation*)))
      (signout-from-oidc default-auth-provider)
      (hard-signout))))
