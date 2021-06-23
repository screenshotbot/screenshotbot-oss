;;;; -*- coding: utf-8 -*-
;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/login
    (:use #:cl
          #:alexandria
          #:markup
          #:nibble
          #:./common
          #:./github-oauth
          #:./google-oauth
          #:../model/github
          #:../model/user
          #:../user-api
          #:../form-errors)
  (:import-from #:../server
                #:defhandler)
  (:import-from #:../installation
                #:standard-auth-provider
                #:installation
                #:auth-provider-signin-form
                #:auth-providers)
  (:export #:auth-header-logo))

(markup:enable-reader)


(markup:deftag auth-header-logo ()
  <a href="/" class= "text-light" >
    <img src= "/assets/images/logo-light.png"
         class= "auth-brand-image" />
    </a>)

(defhandler (nil :uri "/login") ()
  (hex:safe-redirect "/signin"))


(defmethod auth-provider-signin-form ((auth-provider standard-auth-provider) redirect)

  (let ((result (nibble (email password)
                  (signin-post :email email
                               :password password
                               :redirect redirect))))
  <form action=result method= "POST" >
    <div class="form-group mb-3">
      <label for="emailaddress" class= "form-label" >Email address</label>
      <input name= "email"  class="form-control" type="email" id="emailaddress" required="" placeholder="Enter your email">
    </div>

    <div class="form-group mb-3">
      <a href="/forgot-password" class="text-muted float-end"><small>Forgot your password?</small></a>
      <label for="password" class= "form-label" >Password</label>
      <div class="input-group input-group-merge">
        <input name= "password" type="password" id="password" class="form-control" placeholder="Enter your password">
      </div>
    </div>

    <div class="form-group mb-3">
      <div class="custom-control custom-checkbox">
        <input type="checkbox" class="custom-control-input" id="checkbox-signin" checked= "checked" >
          <label class="form-label custom-control-label" for="checkbox-signin">Remember me</label>
      </div>
    </div>

    <div class="form-group mb-0 text-center">
      <button class="btn btn-primary" type="submit"> Log In </button>
    </div>
  </form>))

(deftag signin-get (&key (redirect "/"))
  (assert redirect)
  (let ((signup (nibble ()
                  (signup-get :redirect redirect))))
    <auth-template>

      <div class="account-pages mt-5 mb-5">
            <div class="container">
                <div class="row justify-content-center">
                    <div class="col-lg-5">
                        <div class="card">

                            <!-- Logo -->
                            <div class="card-header pt-4 pb-4 text-center bg-primary">
                            <auth-header-logo />
                            </div>

                            <div class="text-center w-75 m-auto">
                              <h4 class="text-dark-50 text-center mt-0 font-weight-bold">Sign In</h4>
                              <p class="text-muted mb-4">Enter your email address and password to access the dashboard.</p>
                            </div>

                            <div class="card-body p-4">
                              ,@ (loop for auth-provider in (auth-providers (installation))
                                       collect
                                       (auth-provider-signin-form auth-provider redirect))
                            </div> <!-- end card-body -->
                        </div>
                        <!-- end card -->

                        <div class="row mt-3">
                            <div class="col-12 text-center">
                                <p class="text-muted">Don't have an account? <a href=signup class="text-muted ms-1"><b>Sign Up</b></a></p>
                            </div> <!-- end col -->
                        </div>
                        <!-- end row -->

                    </div> <!-- end col -->
                </div>
                <!-- end row -->
            </div>
            <!-- end container -->
        </div>
        <!-- end page -->



      </auth-template>))

(defhandler (nil :uri "/signin" :method :get) (after-logout redirect)
  (declare (ignore after-logout))
  (signin-get :redirect (or redirect "/")))

(defun signin-post (&key email password redirect)
  (let (errors
        (user (user-with-email email)))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check :email (not (str:emptyp email))
             "Please enter an email")
      (check :password (not (str:emptyp password))
             "Please enter a password")
      (check :password (auth:password-hash user)
             "This account appears to use an OAuth (either Google or GitHub)")

      (check :email user
             "No account associated with that email address")
      (when (and user (auth:password-hash user))
       (check :password (auth:check-password user password)
              "Password does not match the given account")))
    (cond
      (errors
       (with-form-errors (:email email :errors errors
                            :was-validated t)
        (signin-get)))
      (t
       (assert (auth:check-password user password))
       (setf (current-user) user)
       (hex:safe-redirect redirect)))))


(defhandler (nil :uri "/signout") ()
  (setf (current-user) nil)
  (hex:safe-redirect "/"))
