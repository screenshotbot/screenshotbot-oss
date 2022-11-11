;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/login/oidc
  (:use #:cl
        #:alexandria
        #:nibble
        #:oidc
        #:screenshotbot/model/user
        #:screenshotbot/login/common)
  (:import-from #:screenshotbot/user-api
                #:user
                #:current-user)
  (:import-from #:bknr.datastore
                #:store-object
                #:with-transaction
                #:persistent-class
                #:hash-index)
  (:import-from #:screenshotbot/model/user
                #:make-user
                #:oauth-user-user
                #:oauth-user-full-name
                #:oauth-user-avatar
                #:oauth-user-email)
  (:import-from #:screenshotbot/login/common
                #:after-create-user)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:export
   #:client-id
   #:client-secret
   #:oidc-provider
   #:issuer
   #:scope
   #:discover
   #:authorization-endpoint
   #:token-endpoint
   #:userinfo-endpoint
   #:access-token-class
   #:access-token-str
   #:oidc-callback
   #:prepare-oidc-user
   #:oidc-user
   #:oauth-user-email
   #:oauth-user-full-name
   #:oauth-user-avatar
   #:find-oidc-user-by-id
   #:find-oidc-users-by-user-id
   #:oauth-user-user
   #:identifier
   #:oidc-provider-identifier
   #:find-existing-oidc-user
   #:end-session-endpoint
   #:update-oidc-user))
(in-package :screenshotbot/login/oidc)

(defclass oidc-provider (abstract-oauth-provider
                         oidc)
  ((identifier :initarg :identifier
               :accessor oidc-provider-identifier))
  (:default-initargs
   :oauth-name "Generic OIDC"
   :callback-endpoint 'oauth-callback))


(defclass oidc-user (store-object)
  ((email :initarg :email
          :accessor oauth-user-email)
   (full-name :initarg :full-name
              :accessor oauth-user-full-name)
   (avatar :initarg :avatar
           :accessor oauth-user-avatar)
   (user-id :initarg :user-id
            :index-type hash-index
            :initform nil
            :index-initargs (:test 'equal)
            :index-reader find-oidc-users-by-user-id)
   (user :initarg :user
         :initform nil
         :accessor oauth-user-user)
   (identifier :initarg :identifier
               :accessor oidc-provider-identifier))
  (:metaclass persistent-class))

;; (token-endpoint (make-instance 'oidc-provider :issuer "https://accounts.google.com"))

(defmethod oauth-signin-link ((auth oidc-provider) redirect)
  (nibble ()
    (push-event :oauth.signin)
    (hex:safe-redirect (make-oidc-auth-link auth redirect))))

(defmethod oauth-signup-link ((auth oidc-provider) redirect)
  (nibble ()
    (push-event :oauth.signup)
    (hex:safe-redirect (make-oidc-auth-link auth redirect))))


(defmethod after-authentication ((auth oidc-provider) &key
                                                       user-id
                                                       email
                                                       full-name
                                                       avatar)
  (log:debug "Got user info ~S" user-id)
  (let ((user (prepare-oidc-user
               auth
               :user-id user-id
               :email email
               :full-name full-name
               :avatar avatar)))
    (setf (current-user) user)))

(defun update-oidc-user (oauth-user &key
                                      (email (error "required"))
                                      (user-id (error "required"))
                                      (full-name (error "required"))
                                      (avatar (error "required")))
  "Update the OIDC user, and return the corresponding USER (i.e. the
user as used in Screenshotbot)"
  (declare (ignore user-id))
  (with-transaction (:initial-setup)
    (setf (oauth-user-email oauth-user) email)
    (setf (oauth-user-full-name oauth-user) full-name)
    (setf (oauth-user-avatar oauth-user) avatar))
  (multiple-value-bind (user first-time-p)
    (or
     (oauth-user-user oauth-user)
     (user-with-email email)
     (values (make-user :email email) t))
    (with-transaction (:ensure-two-way-mapping)
      ;; ensure two way mapping.
      (pushnew oauth-user (oauth-users user))
      (setf (oauth-user-user oauth-user) user))
    ;; what happens if there's another user with the current email?
    ;; For instance, if the Oauth session changed their email address?
    ;; In that case, we ignore and don't make the email change.
    (ignore-errors
     (with-transaction (:set-user-email)
       (setf (user-email user) email)))

    (when first-time-p
     (after-create-user (installation) user))
    user))



(defgeneric prepare-oidc-user (auth &key user-id email full-name avatar)
  (:documentation "Once we have all the information about the user
  that just logged in, convert this into a user in Screenshotbot. You
  may have to look up existing users to figure out which user this is
  mapped to."))

(defmethod find-existing-oidc-user ((auth oidc-provider) user-id)
  (loop for x in (find-oidc-users-by-user-id user-id)
        if (eql (oidc-provider-identifier auth)
                (oidc-provider-identifier x))
          return x))

(defmethod prepare-oidc-user ((auth oidc-provider)
                              &rest all
                              &key user-id email full-name avatar)
  (declare (ignore email full-name avatar))
    (let ((oidc-user (or
                      (find-existing-oidc-user auth (oidc-provider-identifier auth))
                      (make-instance 'oidc-user
                                     :user-id user-id
                                     :identifier (oidc-provider-identifier auth)))))
    (apply 'update-oidc-user
           oidc-user all)))

(defmethod oauth-logo-svg ((auth oidc-provider))
  (declare (ignore auth))
  nil)
