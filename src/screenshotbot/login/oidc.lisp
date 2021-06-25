;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/oidc
    (:use #:cl
          #:alexandria
          #:nibble
          #:./common)
  (:export #:client-id
           #:client-secret
           #:oidc-provider
           #:issuer
           #:scope
           #:discover
           #:authorization-endpoint
           #:token-endpoint
           #:userinfo-endpoint
           #:oidc-callback))

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
   (cached-discovery :initform nil
                     :accessor cached-discovery)))

(defmethod discover ((oidc oidc-provider))
  "Returns an alist of all the fields in the discovery document"
  (or
   (cached-discovery oidc)
   (setf (cached-discovery oidc)
    (let ((url (format nil "~a/.well-known/openid-configuration"
                       (issuer oidc))))
      (with-open-stream (stream
                         (drakma:http-request
                          url
                          :want-stream t
                          :force-binary t))
        (json:decode-json stream))))))

(defmethod authorization-endpoint ((oidc oidc-provider))
  (assoc-value (discover oidc) :authorization--endpoint))

(defmethod token-endpoint ((oidc oidc-provider))
  (assoc-value (discover oidc) :token--endpoint))

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
