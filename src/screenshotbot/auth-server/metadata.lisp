;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/metadata
  (:use #:cl)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:screenshotbot/auth-server/pkce
                #:*supported-code-challenge-methods*)
  (:import-from #:screenshotbot/auth-server/scopes
                #:supported-scope-names)
  (:import-from #:screenshotbot/auth-server/token
                #:supported-grant-types)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:export
   #:authorization-server-metadata)
  (:documentation "OAuth 2.0 Authorization Server Metadata, RFC 8414.

A CLI can point itself at any Screenshotbot installation with just the
hostname and discover the rest from here."))
(in-package :screenshotbot/auth-server/metadata)

(defun %url (path)
  (quri:render-uri
   (quri:merge-uris path (installation-domain *installation*))))

(defun authorization-server-metadata ()
  `(("issuer" . ,(installation-domain *installation*))
    ("authorization_endpoint" . ,(%url "/oauth/authorize"))
    ("token_endpoint" . ,(%url "/oauth/token"))
    ("device_authorization_endpoint" . ,(%url "/oauth/device/code"))
    ("revocation_endpoint" . ,(%url "/oauth/revoke"))
    ("scopes_supported" . ,(supported-scope-names))
    ("response_types_supported" "code")
    ("grant_types_supported" . ,(supported-grant-types))
    ("code_challenge_methods_supported" . ,*supported-code-challenge-methods*)
    ("token_endpoint_auth_methods_supported" "none" "client_secret_basic")
    ("service_documentation" . ,(%url "/documentation/api"))))

(defhandler (nil :uri "/.well-known/oauth-authorization-server" :method :get) ()
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  ;; RFC 8414 §3.2: the metadata is public and cacheable.
  (setf (hunchentoot:header-out :cache-control) "max-age=3600")
  (json:encode-json-alist-to-string (authorization-server-metadata)))
