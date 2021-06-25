;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/google-oauth
    (:use #:cl
          #:alexandria
          #:nibble
          #:./github-oauth
          #:./common
          #:../model/user
          #:./oidc
          #:../user-api
          #:../model/github)
  (:import-from #:../server
                #:defhandler)
  (:import-from #:../secret
                #:secret)
  (:import-from #:bknr.datastore
                #:unique-index
                #:store-object
                #:persistent-class)
  (:export #:google-user
           #:google-access-token
           #:google-oauth-provider
           #:make-google-oauth-link
           #:google-oauth-redirect))

(markup:enable-reader)

(defclass google-oauth-provider (oidc-provider)
  ((oauth-name :initform "Google")
   (issuer :initform "https://accounts.google.com")
   (scope :initform "openid email profile")))

(defclass google-access-token (oauth-access-token)
  ()
  (:metaclass persistent-class))

(defclass google-user (store-object)
  ((email :initarg :email
          :accessor oauth-user-email)
   (full-name :initarg :full-name
              :accessor oauth-user-full-name)
   (avatar :initarg :avatar
           :accessor oauth-user-avatar)
   (user-id :initarg :user-id
            :index-type unique-index
            :initform nil
            :index-initargs (:test 'equal)
            :index-reader %find-google-user-by-id)
   (user :initarg :user
         :initform nil
         :accessor oauth-user-user))
  (:metaclass persistent-class))

(defun make-google-oauth-link (oauth redirect)
  (let* ((auth-uri (quri:uri (authorization-endpoint oauth)))
         (redirect (nibble (code)
                     (%google-oauth-callback oauth code redirect))))

    (setf (quri:uri-query-params auth-uri)
          `(("redirect_uri" . ,(hex:make-full-url hunchentoot:*request* 'oauth-callback))
            ("client_id" . ,(client-id oauth))
            ("state" . ,(format nil "~d" (nibble:nibble-id redirect)))
            ("response_type" . "code")
            ("scope" . ,(scope oauth))))
    (quri:render-uri auth-uri)))

(defmethod oauth-signin-link ((auth google-oauth-provider) redirect)
  (make-google-oauth-link auth redirect))

(defmethod oauth-signup-link ((auth google-oauth-provider) redirect)
  (make-google-oauth-link auth redirect))


(defun make-json-request (url &rest args)
  (multiple-value-bind (stream status-code)
      (apply 'drakma:http-request url
             :want-stream t
             args)
    (with-open-stream (stream stream)
     (case status-code
       (200
        (values
         (json:decode-json stream)
         status-code))
       (otherwise
        (error "Failed to make json request, status code ~a" status-code))))))

(defun prepare-google-user (&rest all &key user-id email full-name avatar)
  (declare (ignore email full-name avatar))
  (let ((google-user (or
                      (%find-google-user-by-id user-id)
                      (make-instance 'google-user
                                     :user-id user-id))))
    (apply 'prepare-oauth-user
           google-user all)))

(defun %google-oauth-callback (auth code redirect)
  (let ((token (oauth-get-access-token
                (token-endpoint auth)
                'google-access-token
                 :client_id (client-id auth)
                 :client_secret (client-secret auth)
                 :code code
                 :redirect_uri (hex:make-full-url
                                hunchentoot:*request*
                                'oauth-callback))))
    (let ((user-info
            (make-json-request (userinfo-endpoint auth)
                               :parameters `(("access_token"
                                              .
                                              ,(access-token-string token))
                                             ("alt" . "json")))))
      (let ((user (prepare-google-user
                   :user-id (assoc-value user-info :sub)
                   :email (assoc-value user-info :email)
                   :full-name (assoc-value user-info :name)
                   :avatar (assoc-value user-info :picture))))
        (setf (current-user) user)
        (hex:safe-redirect redirect)))))


(defmethod oauth-logo-svg ((auth google-oauth-provider))
  (declare (ignore auth))
  <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-google" viewBox="0 0 16 16">
  <path d="M15.545 6.558a9.42 9.42 0 0 1 .139 1.626c0 2.434-.87 4.492-2.384 5.885h.002C11.978 15.292 10.158 16 8 16A8 8 0 1 1 8 0a7.689 7.689 0 0 1 5.352 2.082l-2.284 2.284A4.347 4.347 0 0 0 8 3.166c-2.087 0-3.86 1.408-4.492 3.304a4.792 4.792 0 0 0 0 3.063h.003c.635 1.893 2.405 3.301 4.492 3.301 1.078 0 2.004-.276 2.722-.764h-.003a3.702 3.702 0 0 0 1.599-2.431H8v-3.08h7.545z"/>
  </svg>)
