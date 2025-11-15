;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :oidc/oidc
    (:use #:cl
          #:alexandria)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:util/threading
                #:with-extras)
  (:import-from #:oidc/oauth
                #:make-oauth-url)
  (:export #:oidc
           #:issuer
           #:discover
           #:authorization-endpoint
           #:token-endpoint
           #:userinfo-endpoint
           #:end-session-endpoint
           #:client-id
           #:client-secret
           #:scope
           #:oauth-access-token
           #:access-token-str
           #:oauth-get-access-token
           #:oidc-callback
           #:on-successful-oauth
           #:after-authentication
           #:make-oidc-auth-link
           #:logout-link))

(named-readtables:in-readtable markup:syntax)

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

(defclass oidc ()
  ((issuer :initarg :issuer
           :accessor issuer
           :documentation "The issuer URL, such as
           https://accounts.google.com. We'll use OpenID discovery to
           discover the rest.")
   (client-id :initarg :client-id
              :accessor client-id)
   (client-secret :initarg :client-secret
                  :accessor client-secret)
   (scope :initarg :scope
          :accessor scope
          :initform "openid"
          :documentation "The default scope used for authorization")
   (cached-discovery :initform nil
                     :accessor cached-discovery)
   (via :initarg :via
        :initform nil
        :reader oidc-via
        :documentation "Typically NIL, or perhaps https://screenshotbot.io. This adds a via
point for redirects. This is not a standard part of the OIDC protocol,
but rather a convenience for us to only have one domain listed in the
IdP. For this to work, you must have created an ENTERPRISE-INSTALL
object on prod, other screenshotbot.io will reject it.")))

(defmethod logout-link ((self oidc))
  "RP initiated logout link: https://openid.net/specs/openid-connect-rpinitiated-1_0.html.

  In theory, this should should be implemented using the end_session_endpoint, but for now we're only implementing it for AWS Cognito."
  (error "logout link not implemented for this"))

(defun check-https (url)
  "Our OpenID Connect implementation does not do id-token
  verification. But to still ensure security we have to make sure all
  our calls to the Auth server go ever HTTPS"
  (when url
   (unless (equal "https" (quri:uri-scheme (quri:uri url)))
     (error "Using non https endpoint ~a for authentication" url)))
  url)

(auto-restart:with-auto-restart (:retries 3 :sleep 1)
  (defmethod discover ((oidc oidc))
    "Returns an alist of all the fields in the discovery document"
    (or
     (cached-discovery oidc)
     (setf (cached-discovery oidc)
           (let ((url (format nil "~a/.well-known/openid-configuration"
                              (check-https
                               (issuer oidc)))))
             (let ((ret
                     (json:decode-json-from-string (dex:get url))))
               (log:info "Got ~S~%" ret)
               ret))))))


(defmethod authorization-endpoint ((oidc oidc))
  (check-https
   (assoc-value (discover oidc) :authorization--endpoint)))

(defmethod token-endpoint ((oidc oidc))
  (check-https
   (assoc-value (discover oidc) :token--endpoint)))

(defmethod userinfo-endpoint ((oidc oidc))
  (check-https
   (assoc-value (discover oidc) :userinfo--endpoint)))

(defmethod end-session-endpoint ((oidc oidc))
  (check-https
   (assoc-value (discover oidc) :end--session--endpoint)))

(auto-restart:with-auto-restart (:attempt attempt)
 (defun oauth-get-access-token (token-url &key client_id client_secret code
                                            redirect_uri)
   (assert code)
   (multiple-value-bind (resp token-resp-code)
       (http-request token-url
                     :method :post
                     :want-string t
                     :accept "application/json"
                     :parameters
                     `(("client_id" . ,client_id)
                       ("client_secret" . ,client_secret)
                       ("code" . ,code)
                       ("grant_type" . "authorization_code")
                       ("redirect_uri" . ,redirect_uri)))
     (when (and
            (< attempt 2)
            (eql 502 token-resp-code))
       ;; See T796. In particular, Okta seems to occassional return a
       ;; 502. Are we allowed to retry in this situation? I don't know
       ;; yet, but we'll find out right now. If this doesn't work, a
       ;; more appropriate fix might be to restart the authentication
       ;; flow all over again.
       (warn "Got a 502, retrying token-url.")
       (sleep 1)
       (invoke-restart 'retry-oauth-get-access-token))
     (let* ((resp
              (with-extras (("response" resp)
                            ("response-code" token-resp-code))
                (json:decode-json-from-string resp))))
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
           access-token))))))

(defun make-json-request (url &rest args)
  (multiple-value-bind (stream status-code)
      (apply 'util/request:http-request url
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

(define-condition authentication-error (error)
  ((message :initarg :message
            :reader authentication-error-message)))

(defmethod user-info ((auth oidc) token)
  (make-json-request (userinfo-endpoint auth)
                     :method :post
                     :parameters `(("alt" . "json"))
                     :additional-headers `(("Authorization".
                                                           ,(format nil "Bearer ~a"
                                                                    (Access-token-str token))))))

(defmethod oauth-callback ((auth oidc))
  (error "deprecated"))


(defmethod oidc-callback ((auth oidc) code redirect
                          &key error
                            error-description
                            (error-redirect "/")
                            original-redirect-uri)
  (flet ((error-authenticating (message)
           (hex:safe-redirect
            (nibble ()
              <html>
                <body>
                  There was an error authenticating: ,(progn message)
                  <a href= error-redirect >Go Back</a>
                </body>
              </html>))))
   (cond
     (code
      (let ((token (oauth-get-access-token
                    (token-endpoint auth)
                    :client_id (client-id auth)
                    :client_secret (client-secret auth)
                    :code code
                    :redirect_uri original-redirect-uri)))
        (let ((user-info
                (user-info auth token)))
          (log:debug "Got user info ~S" user-info)
          (verify-userinfo auth user-info)
          (handler-case
              (after-authentication
               auth
               :user-id (assoc-value user-info :sub)
               :email (assoc-value user-info :email)
               :full-name (assoc-value user-info :name)
               :avatar (assoc-value user-info :picture)
               ;; We don't save the token, but in some cases, we need
               ;; the token to fetch the avatar.
               :token token)
            (authentication-error (e)
              (error-authenticating (authentication-error-message e)))))))
     (t
      ;; error the OAuth flow, most likely
      (let ((message (format nil "~a: ~a"
                             error
                             (or error-description "No details provided"))))
        (warn "Oauth failed: ~a" message)
        (error-authenticating
         message))))))

(defmethod verify-userinfo (oidc user-info))

(defgeneric after-authentication (oidc &key
                                         user-id
                                         email
                                         full-name
                                         avatar
                                         token))

(defmethod oidc-callback :after ((auth oidc) code redirect &key &allow-other-keys)
  (declare (ignore code))
  (hex:safe-redirect redirect))

(defmethod make-oidc-auth-link ((oauth oidc) redirect
                                &key (error-redirect "/"))
  (let* ((callback (lambda (&key code error error-description redirect-uri)
                     (oidc-callback oauth code redirect
                                    :error error
                                    :error-description error-description
                                    :error-redirect error-redirect
                                    :original-redirect-uri redirect-uri))))
    (make-oauth-url
     (authorization-endpoint oauth)
     callback
     :via (oidc-via oauth)
     :client_id (client-id oauth)
     :response_type "code"
     :scope (scope oauth))))
