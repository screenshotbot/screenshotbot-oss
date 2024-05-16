;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/email-tasks/settings
  (:use #:cl)
  (:import-from #:screenshotbot/settings-api
                #:settings-template
                #:defsettings)
  (:import-from #:screenshotbot/user-api
                #:current-user
                #:personalp
                #:current-company
                #:company-name)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util/object-id
                #:find-by-oid)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/email-tasks/settings)

(named-readtables:in-readtable markup:syntax)

(defclass email-setting (store-object)
  ((%user :initarg :user
         :index-type hash-index
         :index-reader email-settings-for-user)
   (%company :initarg :company
            :reader company)
   (enabledp :initarg :enabledp
             :accessor emails-enabledp
             :initform t))
  (:metaclass persistent-class))

(defvar *lock* (bt:make-lock))

(defmethod emails-enabled-by-default-p (installation)
  nil)

(defun email-setting (&key user company)
  (flet ((old ()
           (loop for setting in (email-settings-for-user user)
                 if (eql company (company setting))
                   return setting)))
    (or
     (old)
     (bt:with-lock-held (*lock*)
       (or
        (old)
        (make-instance 'email-setting
                        :user user
                        :company company
                        :enabledp (emails-enabled-by-default-p (installation))))))))

(defun save-settings (settings enabledp)
  ;; there's not much in terms of validation to do here.
  (with-transaction ()
    (setf (emails-enabledp settings)
          (if enabledp t)))
  (hex:safe-redirect "/settings/email-tasks"))

(defun get-email-settings (&key (user (current-user))
                             (company (current-company)))

  (let ((company-oid (hunchentoot:parameter "company")))
    (when company-oid
      (let ((company (find-by-oid company-oid)))
        (check-type company company)
        (assert (roles:has-role-p company user t))
        ;; If we're here, we're probably clicking the email control
        ;; link in an email. Since we need to change the setting for
        ;; specific organization, let's change the organization, and
        ;; then redirect back to the settings page.
        (setf (current-company) company)
        (hex:safe-redirect "/settings/email-tasks"))))
  (let* ((settings (email-setting :user user
                                  :company company))
         (save (nibble (enabledp)
                 (save-settings settings enabledp))))
    <settings-template>
      <form action=save method= "POST">
        <div class= "card mt-3">
          <div class= "card-header">
            <h3>Email notifications for tasks</h3>
          </div>
          <div class= "card-body" >
            <p class= "text-muted" >By default, we send notifications to every user on the account. You can control whether your account should receive emails on this page.</p>

            <div class= "form-check">
              <input type= "checkbox" name= "enabledp"
                     id= "enabledp"
                     class= "form-check-input"
                     checked= (if (emails-enabledp settings) "checked")
                     />
              <label for= "enabledp" class= "form-check-label">
                Email me when reports are generated for

                ,(cond
                   ((personalp company)
                    "my organization")
                   (t
                    (company-name company)))
              </label>
            </div>
          </div>
          <div class= "card-footer">
            <input type= "submit" class= "btn btn-primary" value= "Save" />
          </div>
        </div> <!-- card -->
      </form>
    </settings-template>))

(defsettings email-tasks
  :name "email-tasks"
  :title "Email Tasks"
  :section :tasks
  :handler 'get-email-settings)
