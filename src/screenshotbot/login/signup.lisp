;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/signup
    (:use #:cl
          #:alexandria
          #:markup
          #:nibble
          #:./login
          #:./common
          #:./google-oauth
          #:../user-api
          #:../form-errors
          #:../model/user
          #:../model/invite
          #:../model/company
          #:../ignore-and-log-errors
          #:./github-oauth
          #:./populate)
  (:import-from #:../server
                #:*disable-mail*
                #:defhandler)
  (:import-from #:../installation
                #:auth-providers
                #:mailer*
                #:installation
                #:auth-provider-signup-form
                #:standard-auth-provider)
  (:import-from #:../mailer
                #:send-mail)
  (:import-from #:util
                #:oid
                #:find-by-oid)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:export #:signup-get
           #:signup-post))


(markup:enable-reader)

(defmethod auth-provider-signup-form ((auth-provider standard-auth-provider) invite-code
                                      plan
                                      redirect)
  (let ((post (nibble (email password full-name accept-terms-p plan)
                (signup-post
                 :email email
                 :password password
                 :full-name full-name
                 :accept-terms-p accept-terms-p
                 :plan plan
                 :redirect redirect))))
    <form action=post method= "POST" >

      <input type= "hidden" name= "invite-code" value=invite-code />
      <input type= "hidden" name= "plan" value=plan />
      <div class="form-group mb-3">
        <label for="fullname" class= "form-label" >Full Name</label>
        <input name= "full-name" class="form-control" type="text" id="fullname" placeholder="Enter your name" required= "required" >
      </div>

      <div class="form-group mb-3">
        <label for="email" class= "form-label" >Email address</label>
        <input name= "email" class="form-control" type="email" id="email" required= "required" placeholder="Enter your email"
               value=
               (unless (str:emptyp invite-code)
                                               (let ((invite (invite-with-code invite-code)))
               (when invite (invite-email invite))))
               >
      </div>

      <div class="form-group mb-3">
        <label for="password" class= "form-label" >Password</label>
        <div class="input-group input-group-merge">
          <input name= "password"  type="password" id="password" class="form-control" placeholder="Enter your password">
        </div>
      </div>



      <div class="form-group mb-3">
        <div class="custom-control custom-checkbox">
          <input name= "accept-terms-p" type="checkbox" class="custom-control-input" id="checkbox-signup">
            <label class="form-label custom-control-label" for="checkbox-signup">I accept <a href="/terms" class="text-muted">Terms and Conditions</a></label>
        </div>
      </div>

      <div class="form-group mb-0 text-center">
        <button class="btn btn-primary" type="submit" id= "sign-up-submit" > Sign Up </button>
      </div>
    </form>))

(deftag signup-get (&key invite-code plan (redirect "/"))
  (let ((login (nibble ()
                 (signin-get :redirect redirect))))
    <auth-template>
      <div class="account-pages mt-5 mb-5">
        <div class="container">
          <div class="row justify-content-center">
            <div class="col-lg-5">
              <div class="card">
                <!-- Logo-->
                <div class="card-header pt-4 pb-4 text-center bg-primary">
                  <auth-header-logo />
                </div>

                <div class="card-body p-4">

                  <div class="text-center w-75 m-auto">
                    <h4 class="text-dark-50 text-center mt-0 font-weight-bold">Free Sign Up</h4>
                    <p class="text-muted mb-4">Don't have an account? Create your account, it takes less than a minute </p>
                  </div>


                  ,@ (loop for auth-provider in (auth-providers (installation))
                           collect
                           (auth-provider-signup-form auth-provider invite-code
                                                      plan
                                                      redirect))
                </div> <!-- end card-body -->
              </div>
              <!-- end card -->

              <div class="row mt-3">
                <div class="col-12 text-center">
                  <p class="text-muted">Already have account? <a href=login class="text-muted ml-1"><b>Log In</b></a></p>
                </div> <!-- end col-->
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

(defhandler (signup-get-page :uri "/signup" :method :get) (invite-code
                                                      plan)
  (signup-get :invite-code invite-code :plan plan))

(defun signup-post (&key email password full-name accept-terms-p plan redirect)
  (let ((errors))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check :password (and password (>= (length password) 8))
             "Please use at least 8 letters for the password")
      (check :full-name
             (not (str:emptyp (str:trim full-name)))
             "We would really like you to introduce yourself!")
      (check :email
             (clavier::valid-email-address-p email)
             "That doesn't look like a valid email address")
      (check :email
             (not (user-with-email (string-downcase email)))
             "That email address is already in use.")
      (check :accept-terms-p
             accept-terms-p
             "Please accept the terms and conditions to continue")
      (cond
        (errors
         (with-form-errors (:email email :password password
                            :full-name full-name
                            :errors errors
                            :accept-terms-p accept-terms-p
                            :was-validated t)
          (signup-get
           :plan plan)))
        (t
    ;; everything looks good, let's create our user
         (let ((user (make-instance 'user
                                     :full-name full-name
                                     :email email)))
           (with-transaction ()
             (setf (auth:user-password user)
                   password))
           (setf (current-user) user)

           (with-transaction ()
             (setf (unaccepted-invites user)
                   (invites-with-email email)))

           (let ((confirmation (make-instance 'email-confirmation-code
                                               :user user)))
             (send-signup-confirmation (user-email user)
                                       (user-first-name user)
                                       confirmation))
           (ignore-and-log-errors ()
                                  (populate-company (user-personal-company user))))
         (cond
           ((string= (string-upcase plan) (string :professional))
            (hex:safe-redirect "/upgrade"))
           (t
            (hex:safe-redirect redirect))))))))

(defhandler (confirm-email :uri "/confirm-email/:id/:code" :method :get)
            (code id)
  (handler-case
      (let ((confirmation (find-by-oid id)))
        (unless (string= code (secret-code confirmation))
          (error "secret code doesn't match"))

        (cond
          ;; todo: handle expired links
          (t
           (finish-confirmation confirmation)
           (with-transaction ()
            (setf (confirmation-confirmed-p confirmation) t))
           <auth-template>
             <section>
               <div class= "container full-height">
                 Thank you, your email has been confirmed.
                 <a href= "/">Click here to go back to the home page.</a>
               </div>
             </section>
           </auth-template>)))))


(defun render-signup-confirmation (first-name
                                   confirmation
                                   &aux (confirmation-link
                                         (hex:make-full-url hunchentoot:*request*
                                                             'confirm-email
                                                             :id (oid confirmation)
                                                             :code (secret-code confirmation))))
  <html>
  <body>
  <p>Hi ,(progn first-name),</p>

  <p>Thank you for setting up your Screenshotbot account.</p>

  <p>Please <a href=confirmation-link >click here</a> to confirm your email address, and I will assist you with regressions momentarily.</p>

  <p>Your friendly neighborhood bot,<br />
  Screenshotbot</p>

  <p> (P.S. If you reply to this mail, one of our human support staff will be happy to respond!)</p>

  </body>
  </html>)

(defun send-signup-confirmation (email first-name confirmation)
  (unless *disable-mail*
    (send-mail (mailer*)
     :to email
     :subject "Welcome to Screenshotbot"
     :html-message (render-signup-confirmation first-name confirmation))))

(defhandler (nil :uri "/send-test-confirmation") ()
  (let ((conf (make-instance 'email-confirmation-code
                             :email "arnstein87@gmail.com")))
   (send-signup-confirmation "arnstein87@gmail.com" "Arnold" conf)))
