;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/oidc
    (:use #:cl
          #:alexandria
          #:nibble
          #:../model/user
          #:./common)
  (:import-from #:../user-api
                #:user
                #:current-user)
  (:import-from #:bknr.datastore
                #:store-object
                #:with-transaction
                #:persistent-class
                #:hash-index)
  (:import-from #:../model/user
                #:oauth-user-user
                #:oauth-user-full-name
                #:oauth-user-avatar
                #:oauth-user-email)
  (:export #:client-id
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
           #:update-oidc-user))

(defclass oauth-access-token ()
  ((access-token :type (or null string)
                 :initarg :access-token
                 :accessor access-token-str)
   (expires-in :type (or null integer)
               :initarg :expires-in)
   (refresh-token :type (or null string)
                  :initarg :refresh-token)
   (refresh-token-expires-in :type (or null integer)
                             :initarg :refresh-token-expires-in)
   (scope :type (or null string)
          :initarg :scope)
   (token-type :type (or null string)
               :initarg :token-type)))

(defclass oidc-provider (abstract-oauth-provider)
  ((oauth-name :initform "Generic OIDC")
   (client-id :initarg :client-id
              :accessor client-id)
   (client-secret :initarg :client-secret
                  :accessor client-secret)
   (issuer :initarg :issuer
           :accessor issuer
           :documentation "The issuer URL, such as
           https://accounts.google.com. We'll use OpenID discovery to
           discover the rest.")
   (scope :initarg :scope
          :accessor scope
          :initform "openid"
          :documentation "The default scope used for authorization")
   (identifier :initarg :identifier
               :accessor oidc-provider-identifier)
   (cached-discovery :initform nil
                     :accessor cached-discovery)))

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
   (identifier :initarg :identitifer
               :accessor oidc-provider-identifier))
  (:metaclass persistent-class))

(defmethod discover ((oidc oidc-provider))
  "Returns an alist of all the fields in the discovery document"
  (or
   (cached-discovery oidc)
   (setf (cached-discovery oidc)
         (let ((url (format nil "~a/.well-known/openid-configuration"
                            (check-https
                             (issuer oidc)))))
      (json:decode-json-from-string (dex:get url))))))


(defun check-https (url)
  "Our OpenID Connect implementation does not do id-token
  verification. But to still ensure security we have to make sure all
  our calls to the Auth server go ever HTTPS"
  (unless (equal "https" (quri:uri-scheme (quri:uri url)))
    (error "Using non https endpoint ~a for authentication" url))
  url)

(defmethod authorization-endpoint ((oidc oidc-provider))
  (check-https
   (assoc-value (discover oidc) :authorization--endpoint)))

(defmethod token-endpoint ((oidc oidc-provider))
  (check-https
   (assoc-value (discover oidc) :token--endpoint)))

(defmethod userinfo-endpoint ((oidc oidc-provider))
  (assoc-value (discover oidc) :userinfo--endpoint))

(defgeneric oidc-callback (auth code redirect))

;; (token-endpoint (make-instance 'oidc-provider :issuer "https://accounts.google.com"))

(defun make-oidc-auth-link (oauth redirect)
  (let* ((auth-uri (quri:uri (authorization-endpoint oauth)))
         (redirect (nibble (code)
                     (oidc-callback oauth code redirect))))

    (setf (quri:uri-query-params auth-uri)
          `(("redirect_uri" . ,(hex:make-full-url hunchentoot:*request* 'oauth-callback))
            ("client_id" . ,(client-id oauth))
            ("state" . ,(format nil "~d" (nibble:nibble-id redirect)))
            ("response_type" . "code")
            ("scope" . ,(scope oauth))))
    (quri:render-uri auth-uri)))

(defmethod oauth-signin-link ((auth oidc-provider) redirect)
  (make-oidc-auth-link auth redirect))

(defmethod oauth-signup-link ((auth oidc-provider) redirect)
  (make-oidc-auth-link auth redirect))

(defun oauth-get-access-token (token-url &key client_id client_secret code
                                                      redirect_uri)
  (with-open-stream (stream (dex:post token-url
                                      :want-stream t
                                      :headers `(("Accept" . "application/json"))
                                      :content `(("client_id" . ,client_id)
                                                 ("client_secret" . ,client_secret)
                                                 ("code" . ,code)
                                                 ("grant_type" . "authorization_code")
                                                 ("redirect_uri" . ,redirect_uri))))
    (let ((resp
            (json:decode-json stream)))
      (log:info "Got response ~s" resp)
      (when (assoc-value resp :error)
        (error "oauth error: ~s" (assoc-value resp :error--description)))
      (flet ((v (x) (assoc-value resp x)))
        (let ((access-token (make-instance 'oauth-access-token
                                           :access-token (v :access--token)
                                           :expires-in (v :expires--in)
                                           :refresh-token (v :refresh--token)
                                           :refresh-token-expires-in (v :refresh--token--expires--in)
                                           :scope (v :scope)
                                           :token-type (v :token--type))))
          access-token)))))



(defun make-json-request (url &rest args)
  (multiple-value-bind (stream status-code)
      (apply 'dex:request url
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


(defmethod oidc-callback ((auth oidc-provider) code redirect)
  (let ((token (oauth-get-access-token
                (token-endpoint auth)
                :client_id (client-id auth)
                 :client_secret (client-secret auth)
                 :code code
                 :redirect_uri (hex:make-full-url
                                hunchentoot:*request*
                                'oauth-callback))))
    (let ((user-info
            (make-json-request (userinfo-endpoint auth)
                               :method :post
                               :content `(("access_token"
                                           .
                                           ,(access-token-str token))
                                          ("alt" . "json")))))
      (let ((user (prepare-oidc-user
                   auth
                   :user-id (assoc-value user-info :sub)
                   :email (assoc-value user-info :email)
                   :full-name (assoc-value user-info :name)
                   :avatar (assoc-value user-info :picture))))
        (setf (current-user) user)
        (hex:safe-redirect redirect)))))


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
  (let ((user (or
               (oauth-user-user oauth-user)
               (user-with-email email)
               (make-instance 'user
                              :email email))))
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

(defmethod prepare-oidc-user ((auth oidc-provider) &key user-id email full-name avatar)
  (declare (ignore user-id email full-name avatar))
  (error "unimplemented"))
