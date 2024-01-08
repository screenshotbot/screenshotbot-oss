;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/signup
  (:use :cl)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:markup/markup
                #:deftag)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/installation
                #:auth-provider-signup-form
                #:auth-providers
                #:installation
                #:mailer*
                #:standard-auth-provider)
  (:import-from #:screenshotbot/login/common
                #:after-create-user
                #:signup-get
                #:signin-get)
  (:import-from #:screenshotbot/mailer
                #:send-mail)
  (:import-from #:screenshotbot/model/invite
                #:invite
                #:invite-code
                #:invite-email
                #:invite-with-code
                #:invites-with-email)
  (:import-from #:screenshotbot/model/user
                #:confirmation-confirmed-p
                #:email
                #:email-confirmation-code
                #:finish-confirmation
                #:make-user
                #:secret-code
                #:user-first-name
                #:user-with-email)
  (:import-from #:screenshotbot/server
                #:*disable-mail*
                #:defhandler
                #:logged-in-p)
  (:import-from #:screenshotbot/user-api
                #:current-user
                #:unaccepted-invites
                #:user
                #:user-email)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:util/store/object-id
                #:find-by-oid
                #:oid)
  (:import-from #:util/throttler
                #:keyed-throttler
                #:throttle!)
  (:import-from #:screenshotbot/login/template
                #:auth-template)
  (:export
   #:signup-get
   #:signup-get-page
   #:signup-post))
(in-package :screenshotbot/login/signup)

(named-readtables:in-readtable markup:syntax)

(defvar *signup-throttler* (make-instance 'keyed-throttler
                                          :tokens 200)
  "Throttles signups by IP address")

(defmethod auth-provider-signup-form ((auth-provider standard-auth-provider) invite-code
                                      plan
                                      redirect)
  (let* ((invite (unless (str:emptyp invite-code)
                   (invite-with-code invite-code)))
         (invite-email (when invite (invite-email invite)))
         (post (nibble (email password full-name accept-terms-p plan)
                (signup-post
                 :email email
                 :password password
                 :full-name full-name
                 :invite invite
                 :accept-terms-p accept-terms-p
                 :plan plan
                 :redirect redirect))))
    <form action=post method= "POST" >

      <input type= "hidden" name= "invite-code" value=invite-code />
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



      <div class="form-check ps-0 mb-3">
          <input name= "accept-terms-p" type="checkbox" class="form-check-input me-3" id="checkbox-signup" />
            <label class="form-check-label" for="checkbox-signup">I accept the <a href="/terms" class="">Terms and Conditions</a></label>
      </div>

      <div class="form-group mb-0 text-center">
        <button class="btn btn-primary" type="submit" id= "sign-up-submit" > Sign Up </button>
      </div>

    </form>))

(deftag or-signup-with ()
    <div class= "or-signup-with" >
        or Sign Up with
    </div>)

(deftag signup-get (&key invite-code plan (redirect "/runs")
                    alert)
  (when invite-code
    (hex:safe-redirect (format nil "/invite/signup/~a" invite-code)))
  (let ((login (nibble ()
                 (signin-get :redirect redirect
                             :alert alert)))
        (invite (when invite-code
                  (invite-with-code invite-code))))
    <auth-template body-class= "signup" >
      <div class="account-pages mt-5 mb-5">
        <div class="container">

          ,(progn alert)
              <div class="card">

                <div class="card-body p-4">

                  <div class="">
                    <h4 class="text-dark-50 mt-0 font-weight-bold">Sign Up</h4>
                  </div>

                  ,@ (let ((len (length (auth-providers (installation)))))
                       (loop for auth-provider in (auth-providers (installation))
                             for idx from 0
                             collect
                             (auth-provider-signup-form auth-provider invite-code
                                                        plan
                                                        redirect)
                             if (and (eql idx 0) (> len 1))
                               collect  <or-signup-with />))
                </div>
              </div>

              <div class="row mt-3">
                <div class="col-12 text-center">
                  <p class="">Already have account? <a href=login class="ml-1"><b>Log In</b></a></p>
                </div>
              </div>
        </div>
      </div>

    </auth-template>))

(defhandler (signup-get-page :uri "/signup" :method :get) (invite-code
                                                           plan)
    (when (logged-in-p)
      (hex:safe-redirect "/runs"))

  (signup-get :invite-code invite-code :plan plan))

(defun signup-post (&key email password full-name accept-terms-p plan redirect
                      invite)
  (throttle! *signup-throttler* :key (hunchentoot:real-remote-addr))
  (let ((errors))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check :password (and password (>= (length password) 8))
             "Please use at least 8 letters for the password")
      (check :full-name
             (util:token-safe-for-email-p full-name)
             "That name looks invalid")
      (check :full-name
             (str:containsp " " (str:trim full-name))
             "Please provide a first and last name")
      (check :full-name
             (< (length full-name) 150)
             "Name is too long")
      (check :password
             (< (length password) 150)
             "Password is too long")
      (check :email
             (< (length email) 150)
             "Password is too long")
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
         (let ((user (make-user
                      :full-name full-name
                      :email email)))
           (with-transaction ()
             (setf (auth:user-password user)
                   password))
           (setf (current-user) user)

           (let ((invites (remove-if #'null
                                     (remove-duplicates
                                      (list*
                                       invite
                                       (invites-with-email email))))))
             (with-transaction ()
               (setf (unaccepted-invites user)
                invites)))

           (let ((confirmation (make-instance 'email-confirmation-code
                                               :user user)))
             (send-signup-confirmation (user-email user)
                                       (user-first-name user)
                                       confirmation))
           (after-create-user (installation) user))

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
                 <a href= "/runs">Click here to go back to the dashboard.</a>
               </div>
             </section>
           </auth-template>)))))


(defun render-signup-confirmation (first-name
                                   confirmation
                                   &key (confirmation-link
                                         (hex:make-full-url hunchentoot:*request*
                                                             'confirm-email
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
  (unless *disable-mail*
    (send-mail (mailer*)
     :to email
     :subject "Welcome to Screenshotbot"
     :html-message (render-signup-confirmation first-name confirmation))))

(defhandler (nil :uri "/send-test-confirmation") ()
  (let ((conf (make-instance 'email-confirmation-code
                             :email "arnstein87@gmail.com")))
   (send-signup-confirmation "arnstein87@gmail.com" "Arnold" conf)))
