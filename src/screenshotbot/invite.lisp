;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/invite
  (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/server
                #:defhandler
                #:*disable-mail*)
  (:import-from #:screenshotbot/model/invite
                #:invite
                #:email-count
                #:invite-code
                #:invite-company
                #:invite-email
                #:inviter
                #:invites-with-email)
  (:import-from #:screenshotbot/model/user
                #:user-with-email
                #:personalp
                #:professionalp
                #:user-companies
                #:unaccepted-invites
                #:user-full-name)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:store-object-with-id)
  (:import-from #:screenshotbot/model/company
                #:company-name
                #:company-invites)
  (:import-from #:screenshotbot/user-api
                #:current-company
                #:current-user)
  (:import-from #:screenshotbot/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:util #:make-url)
  (:import-from #:screenshotbot/installation
                #:mailer
                #:installation)
  (:import-from #:screenshotbot/mailer
                #:send-mail)
  (:export
   #:invite-page
   #:render-invite-page))
(in-package :screenshotbot/invite)

(markup:enable-reader)

(defmethod render-invite-page (installation)
  (%invite-page))

(defhandler (invite-page :uri "/invite" :method :get) ()
  (render-invite-page (installation)))

(defun %invite-page ()
  <simple-card-page form-action= "/invite" >
    <div class= "card-header">
      <h3>Invite Member</h3>
    </div>

    <div class= "form-group">
      <label for= "email">Email address (we'll send them an invite)</label>
      <input type= "email" class= "form-control" name= "email" id= "email"
             placeholder= "melissa@example.com" />
    </div>

    <div class= "card-footer">
      <input type= "submit" class= "btn btn-primary form-control"
             value= "invite" />
    </div>

  </simple-card-page>)

(defhandler (nil :uri "/invite" :method :post) (email)
  (when (personalp (current-company))
    (hex:safe-redirect 'invite-page))
  (let ((errors))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check :email (not (str:emptyp email))
           "We need an email before we can invite")
      (check :email (let ((user (user-with-email email)))
                      (or
                       (not user)
                       (not (member (current-company)
                                    (user-companies user)))))
             "A user with that email is already a part of this organization")
      (check :email (not (invites-with-email email
                                            :company (current-company)))
             ;; todo: resend anyway
             "There's already a pending invite to that email address")
      (cond
        (errors
         (with-form-errors (:email email
                            :errors (reverse errors)
                            :was-validated t)
           (%invite-page)))
        (t
         (let ((invite (make-instance 'invite
                                      :company (current-company)
                                      :inviter (current-user)
  :email email)))
           (let ((user (user-with-email email)))
             (when user
               (with-transaction ()
                (pushnew invite (unaccepted-invites user)))))
           (send-email-for-invite invite)
           (hex:safe-redirect 'invite-successful
                               :email email)))))))

(defhandler (invite-successful :uri "/invite/success/:email") (email)
  <simple-card-page>
    <div class= "card-header">
      <h3>Invite sent</h3>
    </div>

    <p>,(progn email) should've received an email with details on how to sign.
    </p>q

    <div class= "card-footer">
      <a href= (make-url 'invite-page) class= "btn btn-primary" >Invite More People</a>
      <a href= "/" class= "btn btn-secondary">Home</a>
    </div>
  </simple-card-page>)

(defmethod invite-signup-link ((invite invite))
  (hex:make-full-url
                  hunchentoot:*request*
                  "/signup"
                   :invite-code (invite-code invite)))

(defun send-email-for-invite (invite)
  (unless *disable-mail*
    (send-mail
     (mailer (installation))
     :subject (format nil "~a has invited you to join ~a on Screenshotbot"
                      (user-full-name
                       (inviter invite))
                      (company-name
                       (invite-company invite)))
     :to (invite-email invite)
     :html-message
     <html>
       <body>
         <p>You have been invited to participate in your organization's Screenshotbot
           account.</p>

         <p><a href= (invite-signup-link invite)>Click here to sign up or join</a>.</p>

         <p>Your friendly neighborhood bot, <br/>
           Screenshotbot</p>
       </body>
     </html>)
    (with-transaction ()
      (incf (email-count invite)))))

(defhandler (invite-accept  :uri  "/invite/accept") (invite-id)
  (let ((invite (store-object-with-id (parse-integer invite-id))))
    (when (member invite (unaccepted-invites (current-user)))
      ;; Let's do the accept
      (with-transaction ()
        (deletef (unaccepted-invites (current-user)) invite)
        (pushnew
         (invite-company invite)
         (user-companies (current-user)))
        (deletef
         (company-invites (invite-company invite))
         invite))
      (Setf (current-company) (invite-company invite))))
  (hex:safe-redirect "/"))
