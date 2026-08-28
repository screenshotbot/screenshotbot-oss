;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client
  (:use #:cl)
  (:nicknames :auth-server.client)
  (:import-from #:auth-server/client/conditions
                #:client-error
                #:oauth-error)
  (:import-from #:auth-server/client/discovery
                #:discover
                #:server-metadata)
  (:import-from #:auth-server/client/flows
                #:*announce*
                #:authorization-code-flow
                #:device-flow
                #:refresh-tokens
                #:revoke-token)
  (:import-from #:auth-server/client/http
                #:field
                #:get-json)
  (:import-from #:auth-server/client/token-store
                #:access-token-string
                #:default-token-file
                #:forget-tokens
                #:load-tokens
                #:refresh-token-string
                #:save-tokens
                #:token-scope
                #:token-usable-p)
  (:export
   #:oauth-client
   #:client-host
   #:client-id
   #:client-scope
   #:client-token-file
   #:client-metadata
   #:access-token
   #:sign-in
   #:sign-out
   #:call-api
   #:*announce*
   #:client-error
   #:oauth-error
   #:+default-client-id+)
  (:documentation "The public face of :auth-server.client.

Typical use is one call:

    (let ((client (make-instance 'oauth-client
                                 :host \"https://screenshotbot.io\")))
      (call-api client \"/api/run/ABC123\"))

which signs in if it has to, refreshes if it can, and otherwise reuses
what's cached."))
(in-package :auth-server/client)

(defparameter +default-client-id+ "screenshotbot-cli"
  "The built-in public client. A public client_id identifies but does not
authenticate, so there is nothing secret about this value.")

(defclass oauth-client ()
  ((host :initarg :host
         :initform "https://screenshotbot.io"
         :reader client-host)
   (client-id :initarg :client-id
              :initform +default-client-id+
              :reader client-id)
   (scope :initarg :scope
          :initform "api:read"
          :reader client-scope)
   (token-file :initarg :token-file
               :initform nil
               :reader %token-file)
   (flow :initarg :flow
         :initform :browser
         :reader client-flow
         :documentation ":BROWSER for the loopback redirect, :DEVICE for RFC 8628.")
   (open-browser-p :initarg :open-browser-p
                   :initform t
                   :reader client-open-browser-p)
   (use-discovery-p :initarg :use-discovery-p
                    :initform t
                    :reader client-use-discovery-p)
   (timeout :initarg :timeout
            :initform 300
            :reader client-timeout)
   (%metadata :initform nil
              :accessor %metadata))
  (:documentation "Everything needed to obtain a token for one installation."))

(defmethod client-token-file ((self oauth-client))
  (or (%token-file self) (default-token-file)))

(defmethod client-metadata ((self oauth-client))
  "The server's endpoints, discovered once and remembered."
  (or (%metadata self)
      (setf (%metadata self)
            (discover (client-host self)
                      :use-discovery (client-use-discovery-p self)))))

(defmethod sign-in ((self oauth-client))
  "Run a full interactive sign-in, replacing anything cached."
  (let ((response
          (ecase (client-flow self)
            (:browser
             (authorization-code-flow (client-metadata self)
                                      :client-id (client-id self)
                                      :scope (client-scope self)
                                      :open-browser (client-open-browser-p self)
                                      :timeout (client-timeout self)))
            (:device
             (device-flow (client-metadata self)
                          :client-id (client-id self)
                          :scope (client-scope self)
                          :open-browser (client-open-browser-p self)
                          :timeout (client-timeout self))))))
    (save-tokens (client-token-file self) (client-host self) response)))

(defmethod %refresh ((self oauth-client) tokens)
  "Trade a refresh token for a new pair, or NIL if the server won't.

A refresh failing is not fatal -- the grant may just have been revoked --
so the caller falls back to signing in again."
  (let ((refresh-token (refresh-token-string tokens)))
    (when refresh-token
      (handler-case
          (save-tokens (client-token-file self)
                       (client-host self)
                       (refresh-tokens (client-metadata self)
                                       :client-id (client-id self)
                                       :refresh-token refresh-token))
        (client-error (e)
          (log:debug "Refresh failed, will sign in again: ~a" e)
          nil)))))

(defmethod access-token ((self oauth-client) &key force-new)
  "A usable bearer token: from the cache, from a refresh, or from a sign-in."
  (let ((cached (unless force-new
                  (load-tokens (client-token-file self) (client-host self)))))
    (access-token-string
     (or
      (when (and cached (token-usable-p cached))
        cached)
      (when cached
        (%refresh self cached))
      (sign-in self)))))

(defmethod sign-out ((self oauth-client))
  "Revoke the cached grant at the server and drop the local copy.

Both halves matter: deleting the file alone would leave a live refresh
token on the server."
  (let ((cached (load-tokens (client-token-file self) (client-host self))))
    (when cached
      (ignore-errors
       (revoke-token (client-metadata self)
                     :client-id (client-id self)
                     :token (or (refresh-token-string cached)
                                (access-token-string cached))))
      (forget-tokens (client-token-file self))
      t)))

(defmethod call-api ((self oauth-client) path &key force-new)
  "GET PATH from the installation with a bearer token attached."
  (get-json (quri:render-uri
             (quri:merge-uris path (string-right-trim "/" (client-host self))))
            :bearer (access-token self :force-new force-new)))
