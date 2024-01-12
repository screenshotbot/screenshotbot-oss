;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; CAREFUL: BKNR.DATASTORE CLASSES IN HERE!
(defpackage :screenshotbot/login/oidc
  (:use :cl)
  (:import-from #:auth
                #:current-user
                #:user-email
                #:oauth-user-avatar
                #:oauth-user-email
                #:oauth-user-full-name)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object
                #:with-transaction)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:oidc/oidc
                #:after-authentication
                #:logout-link
                #:make-oidc-auth-link
                #:oidc)
  (:import-from #:screenshotbot/login/common
                #:abstract-oauth-provider
                #:after-create-user
                #:oauth-callback
                #:oauth-logo-svg
                #:oauth-signin-link
                #:oauth-signup-link)
  (:import-from #:screenshotbot/model/user
                #:email ;; TODO: goes away
                #:make-user
                #:oauth-user-user
                #:oauth-users)
  (:import-from #:screenshotbot/user-api
                #:user #| TODO: goes away |#)
  (:import-from #:util/events
                #:push-event)
  (:import-from #:util/store/store
                #:with-class-validation)
  (:import-from #:core/installation/auth
                #:find-user)
  (:export
   #:access-token-class
   #:access-token-str
   #:authorization-endpoint
   #:client-id
   #:client-secret
   #:discover
   #:end-session-endpoint
   #:find-existing-oidc-user
   #:find-oidc-user-by-id
   #:find-oidc-users-by-user-id
   #:identifier
   #:issuer
   #:oauth-user-avatar
   #:oauth-user-email
   #:oauth-user-full-name
   #:oauth-user-user
   #:oidc-callback
   #:oidc-provider
   #:oidc-provider-identifier
   #:oidc-user
   #:prepare-oidc-user
   #:scope
   #:token-endpoint
   #:update-oidc-user
   #:userinfo-endpoint))
(in-package :screenshotbot/login/oidc)

(defclass oidc-provider (abstract-oauth-provider
                         oidc)
  ((identifier :initarg :identifier
               :accessor oidc-provider-identifier)
   (expiration-seconds :initarg :expiration-seconds
                       :initform nil
                       :reader expiration-seconds))
  (:default-initargs
   :oauth-name "Generic OIDC"
   :callback-endpoint 'oauth-callback))


(with-class-validation
  (defclass oidc-user (store-object)
    ((email
      :initarg :old-email-slot
      :documentation "Old email slot. After a few restarts, it should all be unbound and should be safe to delete.")
     (%email :initarg :email
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
     (user :initarg :old-user-slot
           :documentation "Old user slot. After a few restarts, it should all be unbound and should be safe to delete")
     (%user :initarg :user
            :initform nil
            :accessor oauth-user-user)
     (identifier :initarg :identifier
                 :accessor oidc-provider-identifier))
    (:metaclass persistent-class)))

(defmacro define-slot-renames (&rest slots)
  `(progn
     (defmethod bknr.datastore:convert-slot-value-while-restoring ((self oidc-user)
                                                                   slot-name
                                                                   value)
       (assert (symbolp slot-name))
       (cond
         ,@ (loop for (slot nil nil) in slots
                  collect
                  `((string= ,(str:substring 1 nil (string slot)) (string slot-name))
                    (log:info "Renaming slot ~a for ~a" ,(str:substring 1 nil (string slot)) self)
                    (call-next-method self ',slot value)))
            (t
             (call-next-method))))

     ,@(loop for (nil old-slot acc) in slots
              collect
              `(defmethod ,acc :around ((self oidc-user))
                 (cond
                   ((slot-boundp self ',old-slot)
                    (slot-value self ',old-slot))
                   (t
                    (call-next-method)))))

     ,@(loop for (nil old-slot acc) in slots
             collect
             `(defmethod (setf ,acc) :after (val (self oidc-user))
               (slot-makunbound self ',old-slot)
                val))))

(define-slot-renames
    (%email email oauth-user-email)
  (%user user oauth-user-user))

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
    (setf (current-user :expires-in (expiration-seconds auth)) user)))

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
     (find-user *installation* :email email)
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
      (after-create-user *installation* user))
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


(defmethod logout-link ((self oidc-provider))
  "See https://docs.aws.amazon.com/cognito/latest/developerguide/logout-endpoint.html.

In particular this means that /cognito/logout-confirmation must be in your 'Allowed sign-out URLs'"
  nil)
