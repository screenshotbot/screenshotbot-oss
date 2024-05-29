;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/forgot-password
  (:use :cl)
  (:nicknames :auth/login/forgot-password)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:markup/markup
                #:deftag)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/login/common
                #:auth-template
                #:ip-throttler)
  (:import-from #:screenshotbot/mailer
                #:mailer*
                #:send-mail)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:util/throttler
                #:throttle!)
  (:import-from #:core/installation/installation
                #:*installation*))
(in-package :screenshotbot/login/forgot-password)

(defvar *throttler* (make-instance 'ip-throttler))

(named-readtables:in-readtable markup:syntax)

(defclass change-password-request ()
  ((used-up-p :initform nil
              :initarg :used-up-p
              :accessor used-up-p)))

(deftag forgot-password-confirmation (&key email)
  <auth-template simple=t >
    <div class= "container">
      <div class= "card">
        <div class= "card-body">
          <p>We've sent an email to ,(progn email). Please click on the link in that email.</p>
          <p>The link expires in 24 hours.</p>
        </div>
    </div>
    </div>
  </auth-template>)

(defun finish-password-reset (&key user req password confirm-password)
  (let ((errors))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check :password (not (str:emptyp password))
             "Please enter a new password")
      (check :password (>= (length password) 8)
             "Password needs to be at least 8 letters long")
      (check :confirm-password (equal password confirm-password)
             "Passwords don't match")
      (check nil (not (used-up-p req))
             "The password reset code has expired or has already been")
      (log:info "Password errors: ~s" errors)
      (cond
        (errors
         (with-form-errors (:errors errors
                            :was-validated t)
           (reset-password-after-confirmation :user user
                                              :req req)))
        (t
         (with-transaction ()
          (setf (auth:user-password user)
                password))
         (setf (used-up-p req) t)
         <auth-template>
           <div class= "container">
             <p>Your password has changed. <a href= "/login">Go back to Login</a>.</p>
           </div>
         </auth-template>)))))

(deftag reset-password-after-confirmation (&key user req)
  (cond
    ((used-up-p req)
     <auth-template simple=t >
       <p>That code has expired or has already been used</p>
     </auth-template>)
    (t
     (let ((finish-reset (nibble (password confirm-password)
                           (finish-password-reset :user user
                                                  :req req
                                                  :password password
                                                  :confirm-password confirm-password))))
       <auth-template simple=t >
         <div class= "container">
           <form method= "POST" action=finish-reset >
             <div class= "card">
               <div class= "card-body">
                 <div class= "form-group mb-3">
                   <label for= "password">Password</label>
                   <input type= "password" id= "password" name= "password"
                          class= "form-control" />
                 </div>
                 <div class= "form-group mb-3">
                   <label for= "password">Confirm Password</label>
                   <input type= "password" id= "confirm-password" name= "confirm-password"
                          class= "form-control" />
                 </div>
               </div>
               <div class= "card-footer">
                 <input type= "submit" class= "btn btn-primary form-control" value= "Update Password" />
               </div>
             </div>
           </form>
         </div>
       </auth-template>))))

(defun password-recovery-mail (&key confirm)
  <html>
    <body>
      <a href= (nibble:nibble-full-url confirm) >Click here to reset to your password.</a>
      <p>If you didn't request this change of password, please contact us by replying to this email.</p>
    </body>
  </html>)

(defun forgot-password-page ()
  (let ((submit (nibble (email)
                  (throttle! *throttler*)
                  (let ((user (auth:find-user *installation* :email email)))
                    (cond
                      (user
                       (let* ((request (make-instance 'change-password-request))
                              (confirm (nibble ()
                                         (reset-password-after-confirmation :user user
                                                                            :req request))))
                         (restart-case
                             (send-mail (mailer*)
                              :to email
                              :subject "Password recovery"
                              :html-message (password-recovery-mail :confirm confirm))
                           (redirect-to-change-nibble-link ()
                             (hunchentoot:redirect (nibble:nibble-full-url confirm)))))
                       (forgot-password-confirmation :email email))
                      (t
                       (with-form-errors (:errors `((:email . "No user with that email"))
                                          :was-validated t
                                             :email email)
                         (forgot-password-page))))))))
    <auth-template simple=t >
      <div class= "mt-5 mb-5">
          <div class= "row justify-content-center">
            <div class= "col-lg-12">
              <form action=submit method= "POST">
              <div class= "card">
                <div class= "card-header">
                  <h3>Recover your password</h3>
                </div>
                <div class= "card-body">
                  <div class= "form-group mb-3">
                    <label for= "email" class= "form-label" >Email</label>
                    <input class= "form-control" type= "email" id= "email"
                           name= "email"
                           placeholder= "xyz@example.com" />
                  </div>
                </div>

                <div class= "card-footer">
                  <input type="submit" class= "btn btn-lg btn-primary" value= "Recover Password" />
                </div>
                </div>
              </form>
              </div>
            </div>

        </div>
    </auth-template>))

(hex:def-clos-dispatch ((self auth:auth-acceptor-mixin) "/forgot-password") ()
  (forgot-password-page ))
