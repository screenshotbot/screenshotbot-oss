;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/invite
  (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/server
                #:redirect-home
                #:with-login
                #:defhandler)
  (:import-from #:screenshotbot/model/invite
                #:invite-used-p
                #:invite-with-code
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
                #:unaccepted-invites
                #:user-full-name)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:store-object-with-id)
  (:import-from #:screenshotbot/model/company
                #:company-invitation-role
                #:company-name)
  (:import-from #:screenshotbot/user-api
                #:current-company
                #:current-user)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:util #:make-url)
  (:import-from #:screenshotbot/installation
                #:mailer
                #:installation)
  (:import-from #:screenshotbot/mailer
                #:send-mail)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/plan
                #:plan)
  (:import-from #:auth/model/invite
                #:all-invites)
  (:local-nicknames (#:roles #:auth/model/roles))
  (:export
   #:invite-page
   #:render-invite-page
   #:accept-invite))
(in-package :screenshotbot/invite)

(named-readtables:in-readtable markup:syntax)

(defmethod user-can-invite-p (company user)
  (roles:has-role-p company user
                    (company-invitation-role company)))

(defmethod render-invite-page (installation)
  (%invite-page))

(defhandler (invite-page :uri "/invite" :method :get) ()
  (with-login ()
    (cond
      ((not (user-can-invite-p (auth:current-company)
                               (auth:current-user)))
       (no-permission-to-invite))
      (t
       (render-invite-page (installation))))))

(defun no-permission-to-invite ()
  <simple-card-page>
    <p>
    You do not have permission to invite members to this organization.
    Please contact support@screenshotbot.io if you think this is an error.
    </p>
  </simple-card-page>)

(defun %invite-page ()
  <simple-card-page form-action= "/invite" >
    <div class= "card-header">
      <h3>Invite Member</h3>
    </div>

    <div class= "alert alert-danger d-none">
    </div>
    <div class= "form-group mb-3">
      <label class= "form-label" for= "email">Email address (we'll send them an invite)</label>
      <input type= "email" class= "form-control" name= "email" id= "email"
             placeholder= "melissa@example.com" />
    </div>

    <div class= "card-footer">
      <input type= "submit" class= "btn btn-primary form-control"
             value= "Invite" />
    </div>

  </simple-card-page>)

(defmethod can-invite-more-users-p (company plan)
  t)

(defun %user-count (company)
  (+
   (length
    (roles:users-for-company company))
   (length
    (all-invites :company company))))

(defhandler (invite-post :uri "/invite" :method :post) (email)
  (when (personalp (current-company))
    (hex:safe-redirect 'invite-page))
  (let ((errors))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check :email (not (str:emptyp email))
           "We need an email before we can invite")
      (check :email (user-can-invite-p (auth:current-company) (auth:current-user))
             "You do not have the permission to invite a user")
      (check :email (let ((user (user-with-email email)))
                      (or
                       (not user)
                       (not (roles:has-role-p (current-company) user t))))
             "A user with that email is already a part of this organization")
      (check :email (not (invites-with-email email
                                            :company (current-company)))
             ;; todo: resend anyway
             "There's already a pending invite to that email address")
      (when
          (or
           ;; new style:
           (and
            (gk:check :limit-invites (auth:current-company))
            (>= (%user-count (auth:current-company)) 5))
           ;; old style, most likely not being hit
           (not (can-invite-more-users-p (current-company)
                                         (plan))))

        (push
         <span>
           You have reached the limit of users and invites on this account.
           You can <a href= "/settings/members">remove users or invites</a>,
           or you can <a href= "/billing/dashboard">upgrade your plan from here</a>.
         </span>
         errors))
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

(defun invite-alert (invite)
  <div class= "alert alert-info">
    <h4 class= "mb-3 mt-0 pt-0" >Your invitation from ,(user-full-name (inviter invite)) is pending.</h4>
    <p class= "mb-0 pb-0" >Once you create your account you will have an option of accepting the invite.</p>
  </div>)

(defhandler (invite-signup-page :uri "/invite/signup/:invite-code") (invite-code)
  (let ((invite (invite-with-code invite-code)))
    (cond
       ((or (null invite)
            (invite-used-p invite))
        <simple-card-page>
          <p>The invite link has expired. Please ask your administrator to send you a new link.</p>
        </simple-card-page>)
      (t
       (with-login (:signup t :alert (invite-alert invite)
                    :invite invite
                    :ensure-prepared nil)
         (cond
           ((roles:has-role-p (invite-company invite) (current-user) t)
            (setf (auth:current-company) (invite-company invite))
            (redirect-home))
           (t
            (with-transaction ()
              (push invite (unaccepted-invites (current-user)))
              (setf (invite-used-p invite) t))
            (redirect-home))))))))

(defhandler (invite-successful :uri "/invite/success/:email") (email)
  <simple-card-page>
    <div class= "card-header">
      <h3>Invite sent</h3>
    </div>

    <p>,(progn email) should have received an email with details on how to sign-up.
    </p>

    <p>After their account is created, they will be able to add themselves to your organization.</p>

    <div class= "card-footer">
      <a href= (make-url 'invite-page) class= "btn btn-primary" >Invite More People</a>
      <a href= "/settings/members" class= "ms-3" >Manage Invitations</a>
    </div>
  </simple-card-page>)

(defmethod invite-signup-link ((invite invite))
  (hex:make-full-url
                  hunchentoot:*request*
                  'invite-signup-page
                   :invite-code (invite-code invite)))

(defun send-email-for-invite (invite)
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
    (incf (email-count invite))))

(defun accept-invite (invite &key (redirect t)
                               (user (current-user)))
  (%accept-invite invite user)
  (Setf (current-company) (invite-company invite))
  (when redirect
    (redirect-home)))

(defun %accept-invite (invite user)
  (when (or
         (member invite (unaccepted-invites user))
         (string-equal (invite-email invite)
                       (auth:user-email user)))
    ;; Let's do the accept
    (setf (roles:user-role (invite-company invite) user)
          ;; TODO: We should be able to default this to roles:guest
          'roles:standard-member)
    (setf (invite-used-p invite) t)
    (deletef (unaccepted-invites user) invite)))

(defhandler (invite-accept  :uri  "/invite/accept") (invite-id)
  (let ((invite (store-object-with-id (parse-integer invite-id))))
    (accept-invite invite)))
