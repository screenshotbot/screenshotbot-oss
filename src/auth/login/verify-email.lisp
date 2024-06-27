;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/login/verify-email
  (:use #:cl)
  (:import-from #:screenshotbot/mailer
                #:mailer*
                #:send-mail)
  (:import-from #:screenshotbot/login/common
                #:auth-common-header
                #:auth-template)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/login/signup
                #:verify-email
                #:sales-pitch-template)
  (:import-from #:util/form-errors
                #:with-error-builder)
  (:import-from #:util/throttler
                #:throttle!
                #:ip-throttler))
(in-package :auth/login/verify-email)

(named-readtables:in-readtable markup:syntax)

(defvar *code-expiry* (* 20 60))

(defvar *throttler* (make-instance 'ip-throttler
                                   :tokens 300))

(defclass state ()
  ((code :initarg :code
         :reader %code)
   (redirect :initarg :redirect
             :reader %redirect)
   (ts :initform (get-universal-time)
       :reader %ts)
   (email :initarg :email
          :reader %email)))

(defun verify-email (email &key redirect)
  "Triggers an email verification flow, and once the email has been
verified, redirects to the given redirect."
  (assert (eql :post (hunchentoot:request-method*)))
  (throttle! *throttler*)
  (let ((state (make-instance 'state
                              :email email
                              :redirect redirect
                              :code (+ 100000 (secure-random:number 900000)))))
    (send-code-email email (%code state))
    (hex:safe-redirect
     (nibble ()
       (enter-code-screen state)))))

(defun enter-code-screen (state)
  (let ((post (nibble (entered-code)
                (enter-code-screen/post
                 state
                 :entered-code entered-code))))
    <sales-pitch-template  >
      <form method= "POST" action= post >
        <div class= "card border-0 account-card">
          <div class= "card-header">
            <auth-common-header>
              Enter the code we sent to ,(%email state)
            </auth-common-header>
          </div>

          <div class= "card-body">
            <div class= "form-group mb-3">
              <label for= "code" class= "form-label text-muted">Code</label>
              <input name= "entered-code" class= "form-control number-to-text" type= "number" id= "code" placeholder= "000000" required= "required" />
            </div>

            <div class="form-group mb-3 text-center">
              <button class="btn btn-primary" type="submit" id= "sign-up-submit" > Verify Email </button>
            </div>
          </div>
        </div>
      </form>

      <form method="POST" action=(nibble (:method :post) (verify-email (%email state) :redirect (%redirect state))) >
        <div class= "form-group mb-3 text-center">
          <button class= "btn btn-link text-muted" type= "submit" >
            Request a new code
          </button>
        </div>
      </form>


    </sales-pitch-template>))

(defun enter-code-screen/post (state &key entered-code)
  (with-error-builder (:check check :errors errors
                       :form-builder (enter-code-screen state)
                       :success (hex:safe-redirect (%redirect state)))
    (cond
      ((not (equal 6 (length entered-code)))
       (check :entered-code
              nil
              (format nil
                      "The code should be a six digit number that we sent to ~a"
                      (%email state))))
      (t
       (let ((entered-code (parse-integer entered-code :junk-allowed t)))
         (check :entered-code
                (equal entered-code (%code state))
                "The code does not match what we sent over email")
         (check :entered-code (> (+ (%ts state) *code-expiry*) (get-universal-time))
                "The code we sent you has expired. Please request a new code."))))))


(defun send-code-email (email code)
  (send-mail
     (mailer*)
     :to email
     :subject (format nil "~a is your code for Screenshotbot" code)
     :html-message
     <html>
       <body>
         <p>Please enter this code to create your account on Screenshotbot.
         </p>

         <h1>,(progn code)</h1>

         <p>This code expires in 20 minutes.</p>

         <p>Screenshotbot is an Enterprise-grade developer tool. To keep our customers secure and enable collaboration
           we must verify each user's email address.</p>

         <p>If you were not trying to creating an account on Screenshotbot, please ignore this email and do not share this code with any person or website. If you have any questions, please contact us at <a href= "mailto:support@screenshotbot.io">support@screenshotbot.io</a>.</p>
       </body>
     </html>))
