;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/settings/security
  (:use #:cl
        #:alexandria
        #:markup
        #:screenshotbot/model/user
        #:nibble
        #:util/form-errors
        #:screenshotbot/user-api
        #:screenshotbot/settings/settings-template)
  (:import-from #:bknr.datastore
                #:with-transaction))
(in-package :screenshotbot/settings/security)

(markup:enable-reader)

(deftag settings-card (children)
  <div class= "card mt-3">
    ,@children
  </div>)

(deftag settings-card-header (children)
  <div class= "card-header">
    ,@children
  </div>)

(deftag settings-card-footer (children)
  <div class= "card-footer pb-3">
    ,@children
  </div>)

(deftag settings-security-card-body (&key success)
  <div>
        <settings-card-body >
          <div class= "row">
            <div class= "col-md-6">
              <div class= "setting-item-title">Current Password</div>
              <div class= "setting-item-subtitle">Your current password</div>
            </div>

            <div class= "col-md-6">
              <input type= "password" name= "current-password" class= "form-control"
                     />
            </div>
          </div>
        </settings-card-body>

        <settings-card-body >
          <div class= "row">
            <div class= "col-md-6">
              <div class= "setting-item-title">New Password</div>
            </div>

            <div class= "col-md-6">
              <input type= "password" name= "password" class= "form-control"
                     />
            </div>
          </div>
        </settings-card-body>

        <settings-card-body >
          <div class= "row">
            <div class= "col-md-6">
              <div class= "setting-item-title">Verify Password</div>
              <div class= "setting-item-subtitle">Verify your new password</div>
            </div>

            <div class= "col-md-6">
              <input type= "password" name= "verify-password" class= "form-control"
                     />
            </div>
          </div>
        </settings-card-body>

  <settings-card-footer >
    <input type= "submit" class= "btn btn-danger float-right" value= "Change Password" >
    ,(when success
       <div class= "float-right" >Password was successfully changed</div>)

        </settings-card-footer>

  </div>)

(deftag settings-card-body (children &key (class ""))
  <div class= (format nil "card-body ~a" class) >
    ,@children
  </div>)

(deftag settings-security-page (&key success)
    <settings-template>
    <form action= (nibble (current-password
                           password
                           verify-password)
                    (settings-security-page-post current-password
                                                password
                                                verify-password)) method= "post">
      <settings-card>
        <settings-card-header>
          <h4>Change Password</h4>
        </settings-card-header>

        ,(if (auth:password-hash (current-user))
             <settings-security-card-body success=success />
             <settings-card-body>
               This account is associated with an OAuth external account. Please change the password with your OAuth provider.
             </settings-card-body>)
      </settings-card>
    </form>
  </settings-template>)

(defsettings settings-security-page
  :name "security"
  :section nil
  :title "Security"
  :handler (lambda ()
             (settings-security-page)))

(defun settings-security-page-post (current-password
                                    password
                                    verify-password)
  (let ((errors nil))
    (flet ((check (name test message)
             (unless test
               (push (cons name message) errors))))
      (check :current-password
             (auth:check-password (current-user) current-password)
             "Password was incorrect")
      (check :password
             (not (str:emptyp password))
             "Please enter a password")
      (check :password
             (>= (length password) 8)
             "Password should have at least 8 letters")
      (check :verify-password
             (equal password verify-password)
             "The two passwords you entered didn't match"))
    (cond
      (errors
       (with-form-errors (:errors errors
                          :was-validated t)
         (settings-security-page)))
      (t
       (with-transaction ()
        (setf (auth:user-password (current-user))
              password))
       (hex:safe-redirect
        (nibble ()
          (settings-security-page :success t)))))))
