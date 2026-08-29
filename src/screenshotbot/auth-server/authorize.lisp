;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/authorize
  (:use #:cl)
  (:import-from #:core/ui/simple-card-page
                #:simple-card-page)
  (:import-from #:nibble
                #:nibble)
  (:import-from #:screenshotbot/auth-server/errors
                #:oauth-error
                #:oauth-error!
                #:oauth-error-code
                #:oauth-error-description)
  (:import-from #:screenshotbot/auth-server/model
                #:code-string
                #:ensure-builtin-clients
                #:find-oauth-client
                #:make-oauth-code
                #:oauth-client-name
                #:oauth-client-scopes
                #:oauth-client-self-registered-p
                #:oauth-grant
                #:public-client-p
                #:redirect-uri-allowed-p)
  (:import-from #:screenshotbot/auth-server/pkce
                #:valid-code-challenge-method-p)
  (:import-from #:screenshotbot/auth-server/resource-indicators
                #:read-resource)
  (:import-from #:screenshotbot/auth-server/scopes
                #:default-scopes
                #:find-scope
                #:parse-scope-string
                #:render-scope-list
                #:scope-label)
  (:import-from #:screenshotbot/login/common
                #:with-login)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:export
   #:redirect-to-client
   #:consent-page))
(in-package :screenshotbot/auth-server/authorize)

(named-readtables:in-readtable markup:syntax)

(defun redirect-to-client (redirect-uri &rest params)
  "Redirect the user agent back to the client, adding PARAMS to the query.

PARAMS is an alist. Any query the client already put on its redirect_uri
is preserved, as RFC 6749 §3.1.2 requires."
  (let ((uri (quri:uri redirect-uri)))
    (setf (quri:uri-query-params uri)
          (append (quri:uri-query-params uri)
                  (remove nil params :key #'cdr)))
    (hex:safe-redirect (quri:render-uri uri))))

(defun %error-page (title message)
  "Render an error that must not be redirected back to the client.

RFC 6749 §4.1.2.1: if the client_id or the redirect_uri is bad we cannot
trust the redirect target, so the user has to be told directly."
  (setf (hunchentoot:return-code*) 400)
  <simple-card-page max-width= "40rem" >
    <div class= "card-header">
      <h3>,(progn title)</h3>
    </div>
    <p>,(progn message)</p>
    <div class= "card-footer">
      <a href= "/" class= "btn btn-secondary" >Go home</a>
    </div>
  </simple-card-page>)

(defun %validate-client (client-id)
  (cond
    ((str:emptyp client-id)
     (values nil "The application did not identify itself (no client_id)."))
    (t
     (let ((client (find-oauth-client client-id)))
       (cond
         (client
          (values client nil))
         (t
          (values nil (format nil "Unknown application: ~a." client-id))))))))

(defun %requested-scopes (scope client)
  "The scopes to grant, or signal an OAUTH-ERROR."
  (multiple-value-bind (known unknown) (parse-scope-string scope)
    (when unknown
      (oauth-error! "invalid_scope"
                    (format nil "Unsupported scope(s): ~a" (str:join ", " unknown))))
    (let ((requested (or known (default-scopes))))
      (let ((forbidden (set-difference requested (oauth-client-scopes client)
                                       :test #'equal)))
        (when forbidden
          (oauth-error! "invalid_scope"
                        (format nil "This application is not allowed the scope(s): ~a"
                                (str:join ", " forbidden)))))
      requested)))

(defun %validate-pkce (client challenge method)
  "Returns the challenge method to record, or signals an OAUTH-ERROR.

We require PKCE from public clients unconditionally. RFC 8252 §8.1 makes
it mandatory for native apps, and a CLI receiving its code on a loopback
port is exactly the case PKCE exists for."
  (cond
    ((str:emptyp challenge)
     (when (public-client-p client)
       (oauth-error! "invalid_request"
                     "code_challenge is required for public clients (PKCE, RFC 7636)"))
     nil)
    (t
     ;; RFC 7636 §4.3 defaults a missing method to "plain", which we
     ;; don't accept. Be explicit about why.
     (let ((method (or (and (not (str:emptyp method)) method) "plain")))
       (unless (valid-code-challenge-method-p method)
         (oauth-error! "invalid_request"
                       (format nil "Unsupported code_challenge_method: ~a" method)))
       method))))

(defun %grant-and-redirect (&key client redirect-uri scopes state
                              challenge challenge-method resource)
  (let* ((grant (make-instance 'oauth-grant
                               :client client
                               :user (auth:current-user)
                               :company (auth:current-company)
                               :scopes scopes))
         (code (make-oauth-code :grant grant
                                :redirect-uri redirect-uri
                                :challenge challenge
                                :challenge-method challenge-method
                                :resource resource)))
    (redirect-to-client redirect-uri
                        (cons "code" (code-string code))
                        (cons "state" state))))

(defun consent-page (&key client redirect-uri scopes state
                       challenge challenge-method resource)
  (let ((approve (nibble ()
                   (%grant-and-redirect :client client
                                        :redirect-uri redirect-uri
                                        :scopes scopes
                                        :state state
                                        :challenge challenge
                                        :challenge-method challenge-method
                                        :resource resource)))
        (deny (nibble ()
                (redirect-to-client redirect-uri
                                    (cons "error" "access_denied")
                                    (cons "error_description" "The user denied the request")
                                    (cons "state" state))))
        (app-name (or (oauth-client-name client) "An application")))
    <simple-card-page max-width= "40rem" form-action=approve >
      <div class= "card-header">
        <h3>Authorize ,(progn app-name)</h3>
      </div>

      <p><b>,(progn app-name)</b> wants to access your Screenshotbot account
        as <b>,(auth:user-email (auth:current-user))</b>.</p>

      ,(when (oauth-client-self-registered-p client)
         ;; Registration is open, so this name was chosen by whoever
         ;; registered the application rather than by anyone here. Saying
         ;; so is the only thing standing between a user and a consent
         ;; screen that reads "Screenshotbot Official Backup".
         <div class= "alert alert-warning" >
           <b>This application registered itself.</b> Its name has not been
           verified by Screenshotbot. Only continue if you started this
           yourself and recognise where you started it from.
         </div>)

      <p class= "mb-1" >It will be able to:</p>
      <ul>
        ,@ (loop for name in scopes
                 for scope = (find-scope name)
                 if scope
                   collect <li>,(scope-label scope)</li>)
      </ul>

      <p class= "text-muted" >You can revoke this access at any time from the
        <a href= "/oauth/authorizations" >authorized applications</a> page.</p>

      <div class= "card-footer">
        <input type= "submit" class= "btn btn-primary" value= "Authorize" />
        <a href=deny class= "btn btn-secondary" >Cancel</a>
      </div>
    </simple-card-page>))

(defun %authorize (&key response-type client-id redirect-uri scope state
                     code-challenge code-challenge-method
                     (resource-parameters
                      (when (boundp 'hunchentoot:*request*)
                        (hunchentoot:get-parameters*))))
  (ensure-builtin-clients)
  (multiple-value-bind (client client-problem) (%validate-client client-id)
    (cond
      ((null client)
       (%error-page "Invalid request" client-problem))
      ((str:emptyp redirect-uri)
       (%error-page "Invalid request"
                    "The application did not provide a redirect_uri."))
      ((not (redirect-uri-allowed-p client redirect-uri))
       (%error-page "Invalid request"
                    (format nil "~a is not a registered redirect URI for this application."
                            redirect-uri)))
      (t
       ;; The redirect target is trustworthy from here on, so every
       ;; remaining error goes back to the client. RFC 6749 §4.1.2.1.
       (handler-case
           (progn
             (unless (equal "code" response-type)
               (oauth-error! "unsupported_response_type"
                             "Only the authorization code flow is supported"))
             (let ((scopes (%requested-scopes scope client))
                   (challenge-method (%validate-pkce client code-challenge
                                                     code-challenge-method))
                   ;; RFC 8707. Absent is fine -- an audience-less token is
                   ;; still good anywhere that doesn't demand one.
                   (resource (read-resource resource-parameters)))
               (with-login (:allow-url-redirect t)
                 (consent-page :client client
                               :redirect-uri redirect-uri
                               :scopes scopes
                               :state state
                               :challenge code-challenge
                               :challenge-method challenge-method
                               :resource resource))))
         (oauth-error (e)
           (redirect-to-client redirect-uri
                               (cons "error" (oauth-error-code e))
                               (cons "error_description" (oauth-error-description e))
                               (cons "state" state))))))))

(defhandler (nil :uri "/oauth/authorize" :method :get) ()
  (%authorize :response-type (hunchentoot:parameter "response_type")
              :client-id (hunchentoot:parameter "client_id")
              :redirect-uri (hunchentoot:parameter "redirect_uri")
              :scope (hunchentoot:parameter "scope")
              :state (hunchentoot:parameter "state")
              :code-challenge (hunchentoot:parameter "code_challenge")
              :code-challenge-method (hunchentoot:parameter "code_challenge_method")))
