;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/register
  (:use #:cl)
  (:import-from #:screenshotbot/auth-server/cors
                #:allow-cross-origin
                #:preflight)
  (:import-from #:screenshotbot/auth-server/errors
                #:oauth-error!
                #:with-oauth-json-errors)
  (:import-from #:screenshotbot/auth-server/model
                #:oauth-client-id
                #:oauth-client-name
                #:oauth-client-redirect-uris
                #:oauth-client-scopes
                #:oauth-client-secret
                #:oauth-client-created-at
                #:register-oauth-client)
  (:import-from #:screenshotbot/auth-server/scopes
                #:default-scopes
                #:parse-scope-string
                #:render-scope-list)
  (:import-from #:screenshotbot/auth-server/token
                #:+device-code-grant-type+)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/throttler
                #:throttle!
                #:throttler)
  (:export
   #:%register
   #:client-metadata
   #:validate-redirect-uris!
   #:+supported-auth-methods+)
  (:documentation "OAuth 2.0 Dynamic Client Registration, RFC 7591.

An MCP client discovers a server it has never seen before and needs a
client_id on the spot; nobody is going to hand-register Claude Desktop
against every installation. So this endpoint is open, and anything that
comes through it is marked self-registered.

That marking is the point. Open registration means the `client_name` on
the consent screen is chosen by whoever registered, so \"Screenshotbot
Official\" is one POST away. The consent screen renders self-registered
clients distinctly for exactly that reason -- see CONSENT-PAGE."))
(in-package :screenshotbot/auth-server/register)

(defvar *throttler* (make-instance 'throttler :tokens 60)
  "Registration is unauthenticated and creates persistent objects, so it
gets a tighter budget than the token endpoint.")

(defparameter +supported-auth-methods+
  '("none" "client_secret_basic")
  "`none` is a public client using PKCE, which is every MCP client and
every CLI. RFC 7591 §2 defaults to client_secret_basic when the field is
absent.")

(defparameter +supported-grant-types+
  (list "authorization_code" "refresh_token" +device-code-grant-type+))

;; ----------------------------------------------------------------------
;; Reading the request
;; ----------------------------------------------------------------------

(defun %body ()
  (let ((raw (hunchentoot:raw-post-data :force-text t)))
    (when (str:emptyp raw)
      (oauth-error! "invalid_client_metadata" "A JSON body is required"))
    (let ((parsed (handler-case
                      (let ((json:*json-identifier-name-to-lisp* #'identity)
                            (json:*identifier-name-to-key* #'identity))
                        (json:decode-json-from-string raw))
                    (error ()
                      (oauth-error! "invalid_client_metadata"
                                    "The body is not valid JSON")))))
      ;; An empty object decodes to NIL, which is a perfectly good body --
      ;; it just has no redirect_uris, and saying so is VALIDATE-REDIRECT-
      ;; URIS!'s job. Only a body that isn't an object at all fails here.
      (unless (or (null parsed)
                  (and (listp parsed) (every #'consp parsed)))
        (oauth-error! "invalid_client_metadata" "The body must be a JSON object"))
      parsed)))

(defun %field (body name)
  (cdr (assoc name body :test #'equal)))

(defun %string-list (body name)
  "Read a JSON array of strings, tolerating a lone string.

Some clients send `\"redirect_uris\": \"https://...\"`. Rejecting that
would be defensible, but the failure lands on a user trying to connect an
app, a long way from anyone who can read the spec."
  (let ((value (%field body name)))
    (cond
      ((null value) nil)
      ((stringp value) (list value))
      ((listp value)
       (unless (every #'stringp value)
         (oauth-error! "invalid_client_metadata"
                       (format nil "~a must be an array of strings" name)))
       value)
      (t
       (oauth-error! "invalid_client_metadata"
                     (format nil "~a must be an array of strings" name))))))

;; ----------------------------------------------------------------------
;; Validation
;; ----------------------------------------------------------------------

(defun %loopback-host-p (host)
  (member host '("127.0.0.1" "::1" "localhost") :test #'equal))

(defun validate-redirect-uris! (uris)
  "Every redirect URI must be absolute, fragment-free, and either https or
loopback http.

RFC 7591 §2 requires absolute URIs; RFC 6749 §3.1.2 forbids fragments.
Plain http to a real host is refused because the authorization code would
cross the network in the clear -- RFC 8252 §7.3 carves out loopback only
because it never leaves the machine."
  (unless uris
    (oauth-error! "invalid_redirect_uri"
                  "At least one redirect_uri is required"))
  (dolist (uri-string uris)
    (let ((uri (ignore-errors (quri:uri uri-string))))
      (unless (and uri (quri:uri-scheme uri) (quri:uri-host uri))
        (oauth-error! "invalid_redirect_uri"
                      (format nil "Not an absolute URI: ~a" uri-string)))
      (when (quri:uri-fragment uri)
        (oauth-error! "invalid_redirect_uri"
                      (format nil "A redirect_uri may not contain a fragment: ~a"
                              uri-string)))
      (unless (or (equal "https" (quri:uri-scheme uri))
                  (and (equal "http" (quri:uri-scheme uri))
                       (%loopback-host-p (quri:uri-host uri))))
        (oauth-error! "invalid_redirect_uri"
                      (format nil "A redirect_uri must be https, or http on loopback: ~a"
                              uri-string)))))
  uris)

(defun %validate-grant-types! (body)
  (let ((requested (or (%string-list body "grant_types")
                       ;; RFC 7591 §2 default.
                       (list "authorization_code"))))
    (let ((unsupported (set-difference requested +supported-grant-types+
                                       :test #'equal)))
      (when unsupported
        (oauth-error! "invalid_client_metadata"
                      (format nil "Unsupported grant_types: ~a"
                              (str:join ", " unsupported)))))
    requested))

(defun %validate-response-types! (body)
  (let ((requested (or (%string-list body "response_types")
                       (list "code"))))
    (unless (equal '("code") requested)
      (oauth-error! "invalid_client_metadata"
                    "Only the `code` response type is supported"))
    requested))

(defun %validate-auth-method! (body)
  (let ((method (or (%field body "token_endpoint_auth_method")
                    "client_secret_basic")))
    (unless (member method +supported-auth-methods+ :test #'equal)
      (oauth-error! "invalid_client_metadata"
                    (format nil "Unsupported token_endpoint_auth_method: ~a" method)))
    method))

(defun %validate-scopes! (body)
  (let ((scope (%field body "scope")))
    (cond
      ((str:emptyp scope)
       (default-scopes))
      (t
       (multiple-value-bind (known unknown) (parse-scope-string scope)
         (when unknown
           (oauth-error! "invalid_client_metadata"
                         (format nil "Unsupported scope(s): ~a"
                                 (str:join ", " unknown))))
         known)))))

;; ----------------------------------------------------------------------
;; The response
;; ----------------------------------------------------------------------

(defun client-metadata (client &key auth-method grant-types)
  "The registration response for CLIENT, per RFC 7591 §3.2.1."
  `(("client_id" . ,(oauth-client-id client))
    ("client_id_issued_at" . ,(oauth-client-created-at client))
    ,@(when (oauth-client-secret client)
        `(("client_secret" . ,(oauth-client-secret client))
          ;; 0 means it never expires. We have no rotation story, and
          ;; claiming an expiry we don't enforce would be a lie a client
          ;; might act on.
          ("client_secret_expires_at" . 0)))
    ("client_name" . ,(oauth-client-name client))
    ("redirect_uris" . ,(oauth-client-redirect-uris client))
    ("grant_types" . ,grant-types)
    ("response_types" "code")
    ("token_endpoint_auth_method" . ,auth-method)
    ("scope" . ,(render-scope-list (oauth-client-scopes client)))))

(defun %register ()
  (throttle! *throttler* :key (hunchentoot:real-remote-addr))
  (let* ((body (%body))
         (redirect-uris (validate-redirect-uris!
                         (%string-list body "redirect_uris")))
         (grant-types (%validate-grant-types! body))
         (auth-method (%validate-auth-method! body))
         (scopes (%validate-scopes! body)))
    (%validate-response-types! body)
    (let ((client (register-oauth-client
                   :name (%field body "client_name")
                   :redirect-uris redirect-uris
                   :scopes scopes
                   :public (equal "none" auth-method)
                   ;; The whole reason the consent screen treats this
                   ;; client differently.
                   :self-registered t)))
      (setf (hunchentoot:return-code*) hunchentoot:+http-created+)
      (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
      (setf (hunchentoot:header-out :cache-control) "no-store")
      (json:encode-json-alist-to-string
       (client-metadata client :auth-method auth-method
                               :grant-types grant-types)))))

(defhandler (nil :uri "/oauth/register" :method :post) ()
  (allow-cross-origin)
  (with-oauth-json-errors ()
    (%register)))

(defhandler (nil :uri "/oauth/register" :method :options) ()
  (preflight))
