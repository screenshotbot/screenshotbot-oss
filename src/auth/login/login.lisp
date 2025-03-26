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
                #:on-user-sign-in
                #:auth-provider-signin-form
                #:auth-providers
                #:default-oidc-provider)
  (:import-from #:core/installation/installation
                #:site-alert
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
                #:auth-common-header
                #:or-divider
                #:auth-template
                #:ip-throttler
                #:oauth-signin-link
                #:signin-get
                #:signup-get
                #:standard-auth-provider)
  (:import-from #:util/form-errors
                #:with-error-builder
                #:with-form-errors)
  (:import-from #:util/throttler
                #:throttle!)
  (:import-from #:util.cdn
                #:make-cdn)
  (:import-from #:util/events
                #:push-event)
  (:export
   #:auth-header-logo))
(in-package :screenshotbot/login/login)

(markup:enable-reader)

(defparameter *throttler* (make-instance 'ip-throttler
                                         :tokens 120))

(defvar *signin-step1-throttler* (make-instance 'ip-throttler
                                                :tokens 120))

(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/login") ()
  (hex:safe-redirect "/signin"))

(defun fix-input-email ()
  (ps:ps
    (funcall
     (let ((ctr 0))
       (lambda ()
         (let ((el ((ps:@ document get-element-by-id) "disabled-email")))
           (unless (or
                    (equal (ps:@ el value)
                           (ps:@ el dataset actual-value))
                    (> ctr 1000))
             (incf ctr)
             (setf (ps:@ el value)
                   (ps:@ el dataset actual-value)))))))))

(defmethod sign-in-after-email ((auth-provider standard-auth-provider)
                                &key
                                email
                                redirect)
  (let ((result (nibble (password)
                  (signin-post auth-provider
                               :email email
                               :password password
                               :redirect redirect))))
    <auth-template body-class="signin-v2" simple=t >
      <div class= "account-pages mt-5 mb-5" >
          <div class= "card border-0 account-card">
            <div class= "card-header">
              <auth-common-header>
                Log in to your account
              </auth-common-header>

              <div class= "card-body p-4">
                <div class= "alert alert-danger d-none" ></div>
                <div class="form-group mb-3">

                  <input
                    class="form-control disabled-email" disabled= "" value=email autocompletion= "off" readonly= "" id= "disabled-email"
                    data-actual-value=email
                    onchange= (fix-input-email)
                    />

                </div>

                <form action=result method= "POST" >


                  <div class="form-group mb-3 mt-3">
                    <div class="input-group input-group-merge">
                      <input name= "password" type="password" id="password" class="form-control" placeholder="Password" />
                    </div>
                  </div>

                  <input type= "submit" class= "btn btn-primary mb-2" value= "Log In" />
                  <div class= "d-flex justify-content-between">
                    <div class= "mb-3" >
                      <a href= "/signin">Re-enter email</a>
                    </div>
                    <div class= "mb-3" >
                      <a href="/forgot-password" class="links"><small>Forgot your password?</small></a>
                    </div>

                  </div>

                  <div class= "float-end">

                  </div>

                </form>

              </div>
            </div>
          </div>
      </div>
    </auth-template>))

(defmethod sign-in-step1-post ((auth-provider standard-auth-provider) &key email redirect)
  (throttle! *signin-step1-throttler*)
  (with-error-builder (:check check :errors errors
                       :form-builder (signin-get)
                       :success (hex:safe-redirect
                                 (nibble ()
                                   (sign-in-after-email auth-provider
                                                        :email email
                                                        :redirect redirect))))
    (check :email (not (str:emptyp email))
           "Email must be provided")
    (check :email (< (length email) 250)
           "Email is too long")
    (check :email (auth:find-user *installation* :email email)
           (format nil "Could not find a user with email: ~a" email))
    (push-event :signin-attempt :email email)))

(defmethod auth-provider-signin-form ((auth-provider standard-auth-provider) redirect)

  (let ((result (nibble (email)
                  (sign-in-step1-post
                   auth-provider
                   :email email
                   :redirect redirect))))
  <form action=result method= "POST" >
    <div class= "alert alert-danger d-none" ></div>
    <div class="form-group mb-3">
      <input name= "email"  class="form-control" type="email" id="emailaddress" required="" placeholder="Email" autocomplete= "username" />
    </div>

    <div class="form-group mb-3" >
      <button class="btn btn-primary" style= "width: 100%" type="submit"> Next </button>
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
      <auth-template body-class= "signin-v2" simple=t  >

        <div class="account-pages mt-5 mb-5">
          ,(site-alert *installation*)
          ,(progn alert)
          <div class="card border-0 account-card">

            <div class= "card-header">
              <auth-common-header >
                Log in to your account
              </auth-common-header>
            </div>

            <div class="card-body p-4">
              ,@ (loop for auth-provider-cons on (auth-providers *installation*)
                       for count from 0
                       for auth-provider = (car auth-provider-cons)
                       collect
                       (let ((content (auth-provider-signin-form auth-provider redirect)))
                         (if (and (= count 0) (cdr auth-provider-cons))
                             <markup:merge-tag>
                               ,(progn content)
                               <or-divider />
                               <div style="margin-top:2em" />
                             </markup:merge-tag>
                             content)))
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

(defun signin-post (auth-provider &key email password redirect)
  (throttle! *throttler*)
  (let (errors
        (user (auth:find-user *installation* :email email)))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check nil (not (str:emptyp email))
             "Please enter an email")
      (check :password (not (str:emptyp password))
             "Please enter a password")
      (check :password (< (length password) 150)
             "Password too long")
      (check nil user
             (format nil "No account associated with ~a" email))
      (when user
        (check :password (auth:password-hash user)
               "This account appears to use an OAuth (either Google or GitHub)")

        (when (auth:password-hash user)
          (check :password (auth:check-password user password)
                 "Password does not match the given account"))))
    (cond
      (errors
       (with-form-errors (:errors errors
                            :was-validated t)
        (sign-in-after-email auth-provider
                             :email email
                             :redirect redirect)))
      (t
       (assert (auth:check-password user password))
       (on-user-sign-in auth-provider user)
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


(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/account/is-logged-in") ()
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string
   `((:is-logged-in . ,(if (auth:current-user) t :false)))))
