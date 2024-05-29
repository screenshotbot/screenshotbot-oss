;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/signup
  (:use :cl)
  (:nicknames :auth/login/signup)
  (:import-from #:auth
                #:current-user
                #:logged-in-p
                #:user-email
                #:user-first-name)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:core/installation/auth-provider
                #:on-user-sign-in
                #:auth-provider-signup-form
                #:auth-providers)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:markup/markup
                #:deftag)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/login/common
                #:auth-common-header
                #:or-divider
                #:after-create-user
                #:auth-template
                #:signin-get
                #:signup-get
                #:standard-auth-provider)
  (:import-from #:core/installation/mailer
                #:mailer*
                #:send-mail)
  (:import-from #:screenshotbot/model/invite
                #:invite
                #:invite-code
                #:invite-email
                #:invite-with-code
                #:invites-with-email)
  (:import-from #:auth/model/email-confirmation
                #:email-confirmation-code
                #:finish-confirmation
                #:secret-code)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:util/store/object-id
                #:find-by-oid
                #:oid)
  (:import-from #:util/throttler
                #:keyed-throttler
                #:throttle!)
  (:import-from #:screenshotbot/login/login
                #:default-login-redirect)
  (:import-from #:util/events
                #:push-event)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:auth/model/invite
                #:set-user-has-seen-invite)
  (:import-from #:util.cdn
                #:make-cdn)
  (:export
   #:signup-get
   #:signup-post))
(in-package :screenshotbot/login/signup)

(named-readtables:in-readtable markup:syntax)

(defvar *signup-throttler* (make-instance 'keyed-throttler
                                          :tokens 200)
  "Throttles signups by IP address")

(defmethod auth-provider-signup-form ((auth-provider standard-auth-provider) invite
                                      plan
                                      redirect)
  (let* ((invite-email (?. invite-email invite))
         (post (nibble (email password full-name accept-terms-p plan)
                (signup-post
                 auth-provider
                 :email email
                 :password password
                 :full-name full-name
                 :invite invite
                 :accept-terms-p accept-terms-p
                 :plan plan
                 :redirect redirect))))
    <form action=post method= "POST" >

      <input type= "hidden" name= "invite-code" value= (?. invite-code invite) />
      <input type= "hidden" name= "plan" value=plan />
      <div class="form-group mb-3">
        <input name= "full-name" class="form-control" type="text" id="fullname" placeholder="Full Name" required= "required" />
      </div>

      <div class="form-group mb-3">
        <input name= "email" class="form-control" type="email" id="email" required= "required" placeholder="Your Email"
               value=invite-email
               />
      </div>

      <div class="form-group mb-3">
        <div class="input-group input-group-merge">
          <input name= "password"  type="password" id="password" class="form-control" placeholder="Password" />
        </div>
      </div>



      <div class="form-check mb-3">
          <input name= "accept-terms-p" type="checkbox" class="form-check-input" id="checkbox-signup" />
            <label class="form-check-label" for="checkbox-signup">I accept the <a href="/terms" class="">Terms and Conditions</a></label>
      </div>

      <div class="form-group mb-3 text-center">
        <button class="btn btn-primary" type="submit" id= "sign-up-submit" > Sign Up </button>
      </div>

    </form>))

(deftag or-signup-with ()
    <div class= "or-signup-with" >
        or Sign Up with
    </div>)

(markup:deftag sales-pitch ()
  <div class= "sales-pitch" >
    <ul>
      <li><i class="material-icons">smartphone</i>
        <strong>Enterprise-Ready</strong>
        <p>Automate screenshot tests at scale, from 10s of screenshots to 10s of thousands per commit.</p>
      </li>
      <li>
        <i class="material-icons">merge</i>
        <strong>Pull Request integrations</strong>
        <p>See UI changes directly in pull requests.</p>
      </li>
      <li>
        <i class="material-icons">error</i>
        <strong>Catch regressions</strong>
        <p>Find UI regressions before your app ships</p>
      </li>

      <li>
        <i class="material-icons">people</i>
        <strong>Collaborate across disciplines</strong>
        <p>Everyone loves screenshots, even if they don't code. Notify them on Slack or Email.</p>
      </li>
      <li class= "" >
        <i class="material-icons">history</i>
        <strong>Rewind History</strong>
        <p>Bisect UI regressions with just a few clicks</p>
      </li>

      <li>
        <i class="material-icons">code</i>
        <strong>Use libraries of your choice</strong>
        <p>Use your existing iOS, Android or Web screenshot testing libraries</p>
      </li>
</ul>

    <div class= "preview-wrapper">
      <img src= (make-cdn "/assets/images/screenshotbot-preview-screenshot-3.webp") class= "shadow-lg" />
    </div>
  </div>)

(deftag signup-get (&key plan (redirect (default-login-redirect hunchentoot:*request*))
                    invite
                    alert)
  (let ((login (nibble ()
                 (signin-get :redirect redirect
                             :alert alert))))
    <auth-template body-class= "signin-v2" simple=t full-width=t >
      <div class="account-pages mb-5">
        <div class="container">

          <div class= "row g-4">
            <div class= "col-1" />
            <div class= "col-md-10 col-lg-5">
              <div class="card border-0 account-card">

                <div class= "card-header">
                  <auth-common-header>
                    Enter your credentials to create your account
                  </auth-common-header>
                </div>

                <div class="card-body p-4">
                  ,(progn alert)

                  ,@ (let ((len (length (auth-providers *installation*))))
                       (loop for auth-provider in (auth-providers *installation*)
                             for idx from 0
                             collect
                             (auth-provider-signup-form auth-provider invite
                                                        plan
                                                        redirect)
                             if (and (eql idx 0) (> len 1))
                               collect  <or-divider />))
                </div>
              </div>

              <div class="row mt-3">
                <div class="col-12 text-center">
                  <p class="">Already have account? <a href=login class="ml-1"><b>Log In</b></a></p>
                </div>
              </div>
            </div>

            <div class= "col-5 d-none d-lg-block">
              <sales-pitch />
            </div>
          </div>
        </div>
      </div>

    </auth-template>))


(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/signup") (plan)
  (when (logged-in-p)
    (hex:safe-redirect (default-login-redirect hunchentoot:*request*)))

  (signup-get :plan plan))

(defun valid-email-address-p (string)
  "This comes from clavier::valid-email-address-p, but fixed for
bugs. (See corresponding tests.)"
  (not (null
	(ppcre:scan "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,12}$" string))))

(defun validate-name (check-fn full-name)
  (flet ((check (&rest args)
           (apply check-fn args)))
    (check :full-name
           (util:token-safe-for-email-p full-name)
           "That name looks invalid")
    (check :full-name
           (< (length full-name) 150)
           "Name is too long")
    (check :full-name
           (not (str:emptyp (str:trim full-name)))
           "We would really like you to introduce yourself!")))

(defun signup-post (auth-provider
                    &key email password full-name accept-terms-p plan redirect
                      invite)
  (throttle! *signup-throttler* :key (hunchentoot:real-remote-addr))
  (let ((errors))
    (flet ((check (name test message)
             (unless test
               (push-event :signup-failure
                           :test (string test)
                           :email email
                           :full-name full-name
                           :message message)
               (push (cons name message) errors))))
      (check :password (and password (>= (length password) 8))
             "Please use at least 8 letters for the password")

      (validate-name #'check full-name)

      (check :full-name
             (str:containsp " " (str:trim full-name))
             "Please provide a first and last name (we know names sometimes don't have a last name, but unfortunately we need this to prevent bots. Put in a placeholder last name, and you can change it later!)")

      (check :password
             (< (length password) 150)
             "Password is too long")
      (check :email
             (< (length email) 150)
             "Password is too long")
      (check :email
             (valid-email-address-p email)
             "That doesn't look like a valid email address")
      (check :email
             (not (auth:find-user *installation* :email (string-downcase email)))
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
         (let ((user (auth:make-user
                      *installation*
                      :full-name full-name
                      :email email)))
           (with-transaction ()
             (setf (auth:user-password user)
                   password))
           (on-user-sign-in auth-provider user)
           (setf (current-user) user)

           (process-existing-invites user email :current-invite invite)
           (prepare-and-send-email-confirmation user)
           (after-create-user *installation* user))

         (cond
           ((string= (string-upcase plan) (string :professional))
            (hex:safe-redirect "/upgrade"))
           (t
            (hex:safe-redirect redirect))))))))

(defun prepare-and-send-email-confirmation (user)
  "Send an email confirmation to the USER"
  (let ((confirmation (make-instance 'email-confirmation-code
                                     :user user)))
    (send-signup-confirmation (user-email user)
                              (user-first-name user)
                              confirmation)))

(defun process-existing-invites (user email &key current-invite)
  "When the USER is created, the user might have existing invites. Do
any processing related to those invites here.

CURRENT-INVITE is the current invite being used while signing up. This
might not be an existing invite because when you sign up you might use
a different email."
  (when current-invite
    (set-user-has-seen-invite user current-invite))
  (let ((invites (remove-if #'null
                            (remove-duplicates
                             (list*
                              current-invite
                              (invites-with-email email))))))
    (with-transaction ()
      (setf (auth:unaccepted-invites user)
            invites))))

(defun confirmation-success ()
  <auth-template simple=t >
    <section>
      <div class= "container full-height">
        <p>Thank you, your email has been confirmed.</p>
        <p><a href= (default-login-redirect hunchentoot:*request*) >Click here to go back to the dashboard.</a></p>
      </div>
    </section>
  </auth-template>)

(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/confirm-email") (code id)
  (handler-case
      (let ((confirmation (find-by-oid id)))
        (unless (string= code (secret-code confirmation))
          (error "secret code doesn't match"))

        (cond
          ;; todo: handle expired links
          (t
           (finish-confirmation confirmation)
           (confirmation-success))))))


(defun render-signup-confirmation (first-name
                                   confirmation
                                   &key (confirmation-link
                                         (hex:make-full-url hunchentoot:*request*
                                                             "/confirm-email"
                                                             :id (oid confirmation)
                                                             :code (secret-code confirmation))))
   <html>
     <body>
       <p>Hi ,(progn first-name),</p>

       <p>Thank you for setting up your Screenshotbot account.</p>

       <p>
         Please <a href=confirmation-link >click here</a>
         to confirm your email address.
       </p>

       <p>
         If you need help or support please reach out to
         <a href= "mailto:support@screenshotbot.io">support@screenshotbot.io</a>
       </p>

       <p>--Screenshotbot</p>

     </body>
  </html>)

(defun send-signup-confirmation (email first-name confirmation)
  (send-mail (mailer*)
             :to email
             :subject "Welcome to Screenshotbot"
             :html-message (render-signup-confirmation first-name confirmation)))
