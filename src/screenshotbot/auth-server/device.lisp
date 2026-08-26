;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/device
  (:use #:cl)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/auth-server/errors
                #:oauth-error!
                #:with-oauth-json-errors
                #:write-json)
  (:import-from #:screenshotbot/auth-server/model
                #:approve-device-request
                #:deny-device-request
                #:device-client
                #:device-code-string
                #:device-expires-at
                #:device-interval
                #:device-scopes
                #:device-status
                #:device-user-code
                #:ensure-builtin-clients
                #:find-device-request-by-user-code
                #:find-oauth-client
                #:make-device-request
                #:normalize-user-code
                #:oauth-client-name
                #:oauth-client-scopes
                #:oauth-grant)
  (:import-from #:screenshotbot/auth-server/scopes
                #:default-scopes
                #:find-scope
                #:parse-scope-string
                #:scope-label)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:util/throttler
                #:throttle!
                #:throttler)
  (:export
   #:verification-uri)
  (:documentation "The OAuth 2.0 Device Authorization Grant, RFC 8628.

This is the flow for a CLI that can't pop open a browser on the machine
it's running on -- an ssh session, a container, a CI box. The CLI prints
a short code, the user types it into a browser anywhere, and the CLI
polls the token endpoint until they're done."))
(in-package :screenshotbot/auth-server/device)

(named-readtables:in-readtable markup:syntax)

(defvar *throttler* (make-instance 'throttler :tokens 300))

(defun verification-uri ()
  (hex:make-full-url hunchentoot:*request* "/oauth/device"))

;; ----------------------------------------------------------------------
;; The device authorization endpoint (RFC 8628 §3.1, §3.2)
;; ----------------------------------------------------------------------

(defun %device-code ()
  (ensure-builtin-clients)
  (throttle! *throttler* :key (hunchentoot:real-remote-addr))
  (let* ((client-id (hunchentoot:post-parameter "client_id"))
         (client (unless (str:emptyp client-id)
                   (find-oauth-client client-id))))
    (unless client
      (oauth-error! "invalid_client" "Unknown or missing client_id" :status 401))
    (multiple-value-bind (known unknown)
        (parse-scope-string (hunchentoot:post-parameter "scope"))
      (when unknown
        (oauth-error! "invalid_scope"
                      (format nil "Unsupported scope(s): ~a" (str:join ", " unknown))))
      (let ((scopes (or known (default-scopes))))
        (let ((forbidden (set-difference scopes (oauth-client-scopes client)
                                         :test #'equal)))
          (when forbidden
            (oauth-error! "invalid_scope"
                          (format nil "This application is not allowed the scope(s): ~a"
                                  (str:join ", " forbidden)))))
        (let ((request (make-device-request :client client :scopes scopes))
              (verification-uri (verification-uri)))
          (write-json
           `(("device_code" . ,(device-code-string request))
             ("user_code" . ,(device-user-code request))
             ("verification_uri" . ,verification-uri)
             ("verification_uri_complete"
              . ,(format nil "~a?user_code=~a" verification-uri
                         (quri:url-encode (device-user-code request))))
             ("expires_in" . ,(max 0 (- (device-expires-at request)
                                        (get-universal-time))))
             ("interval" . ,(device-interval request)))))))))

(defhandler (nil :uri "/oauth/device/code" :method :post) ()
  (with-oauth-json-errors ()
    (%device-code)))

;; ----------------------------------------------------------------------
;; The user-facing verification page (RFC 8628 §3.3)
;; ----------------------------------------------------------------------

(defun %message-page (title message)
  <simple-card-page max-width= "40rem" >
    <div class= "card-header">
      <h3>,(progn title)</h3>
    </div>
    <p>,(progn message)</p>
    <div class= "card-footer">
      <a href= "/" class= "btn btn-secondary" >Go home</a>
    </div>
  </simple-card-page>)

(defun %enter-code-page (&key alert)
  (let ((submit (nibble (user_code)
                  (%device-verification :user-code user_code))))
    <simple-card-page max-width= "40rem" form-action=submit >
      <div class= "card-header">
        <h3>Connect a device</h3>
      </div>

      ,(when alert
         <div class= "alert alert-danger" >,(progn alert)</div>)

      <p>Enter the code shown by the application you're signing in from.</p>

      <div class= "mb-3" >
        <label for= "user_code" class= "form-label" >Code</label>
        <input type= "text" name= "user_code" id= "user_code" class= "form-control"
               autocomplete= "off" autofocus= "autofocus" placeholder= "XXXX-XXXX" />
      </div>

      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Continue" />
      </div>
    </simple-card-page>))

(defun %approve (request)
  (let ((grant (make-instance 'oauth-grant
                              :client (device-client request)
                              :user (auth:current-user)
                              :company (auth:current-company)
                              :scopes (device-scopes request))))
    (approve-device-request request grant)
    (%message-page
     "Device connected"
     "You can close this window and go back to the application.")))

(defun %device-consent-page (request)
  (let ((approve (nibble ()
                   (%approve request)))
        (deny (nibble ()
                (deny-device-request request)
                (%message-page "Request denied"
                               "The application was not given access to your account.")))
        (app-name (or (oauth-client-name (device-client request)) "An application")))
    <simple-card-page max-width= "40rem" form-action=approve >
      <div class= "card-header">
        <h3>Authorize ,(progn app-name)</h3>
      </div>

      <p><b>,(progn app-name)</b> is asking to access your Screenshotbot account
        as <b>,(auth:user-email (auth:current-user))</b>.</p>

      <p class= "mb-1" >It will be able to:</p>
      <ul>
        ,@ (loop for name in (device-scopes request)
                 for scope = (find-scope name)
                 if scope
                   collect <li>,(scope-label scope)</li>)
      </ul>

      <p class= "text-muted" >Only continue if you just started this from
        code <b>,(device-user-code request)</b>.</p>

      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Authorize" />
        <a href=deny class= "btn btn-secondary" >Cancel</a>
      </div>
    </simple-card-page>))

(defun %device-verification (&key user-code)
  (ensure-builtin-clients)
  (with-login (:allow-url-redirect t)
    (cond
      ((str:emptyp user-code)
       (%enter-code-page))
      (t
       (throttle! *throttler* :key (auth:current-user))
       (let ((request (find-device-request-by-user-code
                       (normalize-user-code user-code))))
         (cond
           ((null request)
            (%enter-code-page :alert "That code isn't valid. Check it and try again."))
           ((< (device-expires-at request) (get-universal-time))
            (%enter-code-page :alert "That code has expired. Start again from the application."))
           ((not (eql :pending (device-status request)))
            (%message-page "Already handled"
                           "That code has already been used."))
           (t
            (%device-consent-page request))))))))

(defhandler (nil :uri "/oauth/device" :method :get) ()
  (%device-verification :user-code (hunchentoot:parameter "user_code")))
