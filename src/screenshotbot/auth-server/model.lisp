;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/model
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:class-instances
                #:delete-object
                #:persistent-class
                #:store-object
                #:store-object-id
                #:with-transaction)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/fset-index
                #:fset-set-index
                #:fset-unique-index
                #:index-least)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:core/api/model/api-key
                #:api-key
                #:api-key-company
                #:api-key-permissions
                #:api-key-user
                #:api-key-user-visible-p
                #:encode-api-token
                #:expired-p
                #:expires-at)
  (:import-from #:screenshotbot/auth-server/pkce
                #:constant-time-equal
                #:random-token)
  (:import-from #:screenshotbot/auth-server/scopes
                #:scopes-permissions)
  (:export
   ;; clients
   #:oauth-client
   #:oauth-client-id
   #:oauth-client-name
   #:oauth-client-secret
   #:oauth-client-redirect-uris
   #:oauth-client-scopes
   #:public-client-p
   #:oauth-client-self-registered-p
   #:find-oauth-client
   #:register-oauth-client
   #:redirect-uri-allowed-p
   #:+cli-client-id+
   #:ensure-builtin-clients

   ;; grants
   #:oauth-grant
   #:grant-client
   #:grant-user
   #:grant-company
   #:grant-scopes
   #:grant-created-at
   #:grant-revoked-p
   #:grant-valid-p
   #:revoke-grant
   #:oauth-grants-for-user
   #:oauth-grants-for-company

   ;; authorization codes
   #:oauth-code
   #:code-string
   #:code-grant
   #:code-redirect-uri
   #:code-challenge
   #:code-challenge-method
   #:code-expires-at
   #:code-consumed-p
   #:code-resource
   #:find-oauth-code
   #:make-oauth-code
   #:consume-oauth-code

   ;; tokens
   #:oauth-access-token
   #:oauth-refresh-token
   #:access-token-grant
   #:access-token-scopes
   #:access-token-resource
   #:refresh-token-resource
   #:device-resource
   #:refresh-token-string
   #:refresh-token-grant
   #:refresh-token-expires-at
   #:refresh-token-revoked-p
   #:find-refresh-token
   #:make-access-token
   #:make-refresh-token
   #:access-token-string
   #:access-token-expires-in
   #:revoke-refresh-token

   ;; device flow
   #:oauth-device-request
   #:device-code-string
   #:device-user-code
   #:device-client
   #:device-scopes
   #:device-status
   #:device-grant
   #:device-expires-at
   #:device-interval
   #:device-last-polled-at
   #:find-device-request
   #:find-device-request-by-user-code
   #:make-device-request
   #:approve-device-request
   #:deny-device-request
   #:consume-device-request
   #:note-device-poll
   #:normalize-user-code

   #:*access-token-ttl*
   #:*refresh-token-ttl*
   #:*authorization-code-ttl*
   #:*device-code-ttl*
   #:cleanup-expired-oauth-objects))
(in-package :screenshotbot/auth-server/model)

(defparameter *authorization-code-ttl* 600
  "Seconds an authorization code stays valid. RFC 6749 §4.1.2 recommends a
maximum of 10 minutes.")

(defparameter *access-token-ttl* 3600
  "Seconds an access token stays valid.")

(defparameter *refresh-token-ttl* (* 90 24 3600)
  "Seconds a refresh token stays valid, if it is never used. Every refresh
mints a new token, so an actively used CLI never hits this.")

(defparameter *device-code-ttl* 900
  "Seconds a device authorization request stays pending. RFC 8628 §3.2.")

(defparameter *device-poll-interval* 5
  "Minimum seconds between device token polls. RFC 8628 §3.5.")

;; ----------------------------------------------------------------------
;; Clients
;; ----------------------------------------------------------------------

(defindex +client-id-index+
  'fset-unique-index
  :slot-name '%client-id)

(with-class-validation
  (defclass oauth-client (store-object)
    ((%client-id :initarg :client-id
                 :index +client-id-index+
                 :index-reader find-oauth-client
                 :reader oauth-client-id)
     (%name :initarg :name
            :initform nil
            :accessor oauth-client-name
            :documentation "Shown to the user on the consent screen.")
     (%secret :initarg :secret
              :initform nil
              :accessor oauth-client-secret
              :documentation "NIL for public clients. CLI tools are always public: a
secret shipped inside a binary on the user's machine isn't a secret.")
     (%redirect-uris :initarg :redirect-uris
                     :initform nil
                     :accessor oauth-client-redirect-uris)
     (%scopes :initarg :scopes
              :initform nil
              :accessor oauth-client-scopes
              :documentation "The scopes this client is allowed to ask for.")
     (%created-at :initarg :created-at
                  :initform nil
                  :reader oauth-client-created-at)
     (%self-registered :initarg :self-registered
                       :initform nil
                       :reader oauth-client-self-registered-p
                       :documentation "True if this client registered itself through RFC 7591
dynamic registration, rather than being created by someone here.

Nobody vetted its name or its redirect URIs, and the consent screen
renders it differently because of that: the name on that screen is the
only thing a user has to go on, and a self-registered client chose it."))
    (:metaclass persistent-class)
    (:default-initargs
     :client-id (random-token 16)
     :created-at (get-universal-time))))

(defmethod public-client-p ((self oauth-client))
  (null (oauth-client-secret self)))

(defun register-oauth-client (&key client-id name redirect-uris scopes
                                (public t) self-registered)
  "Create an OAuth client. Public clients (the default) get no secret and
must use PKCE.

SELF-REGISTERED marks a client that arrived through dynamic registration;
it defaults to NIL so anything created from here or from a REPL counts as
vetted."
  (apply #'make-instance 'oauth-client
         :name name
         :redirect-uris redirect-uris
         :scopes scopes
         :self-registered self-registered
         :secret (unless public (random-token 32))
         (when client-id (list :client-id client-id))))

;;; Redirect URI matching

(defun %loopback-host-p (host)
  (member host '("127.0.0.1" "::1" "localhost") :test #'equal))

(defun %loopback-uri-p (uri)
  (and
   (equal "http" (quri:uri-scheme uri))
   (%loopback-host-p (quri:uri-host uri))))

(defun redirect-uri-allowed-p (client redirect-uri)
  "Does REDIRECT-URI match one of CLIENT's registered URIs?

Matching is exact (RFC 6749 §3.1.2.3) with one exception: for loopback
redirects the port is ignored, because a CLI has to bind whatever
ephemeral port it can get. That is RFC 8252 §7.3."
  (let ((actual (ignore-errors (quri:uri redirect-uri))))
    (when actual
      (loop for registered-string in (oauth-client-redirect-uris client)
            for registered = (ignore-errors (quri:uri registered-string))
            thereis (cond
                      ((null registered)
                       nil)
                      ((and (%loopback-uri-p registered)
                            (%loopback-uri-p actual))
                       (and
                        (equal (quri:uri-host registered) (quri:uri-host actual))
                        (equal (or (quri:uri-path registered) "/")
                               (or (quri:uri-path actual) "/"))))
                      (t
                       (equal registered-string redirect-uri)))))))

;;; The built-in client used by our own CLI tooling.

(defparameter +cli-client-id+ "screenshotbot-cli"
  "The client_id of the first-party CLI. It is a public client, so this
value is not a secret -- it just identifies the application.")

(defvar *builtin-client-lock* (bt:make-lock "oauth-builtin-clients"))

(defun ensure-builtin-clients ()
  "Create the first-party CLI client if it doesn't exist yet.

Called from every OAuth entry point; after the first call this is just an
index lookup."
  (or
   (find-oauth-client +cli-client-id+)
   (bt:with-lock-held (*builtin-client-lock*)
     (or
      (find-oauth-client +cli-client-id+)
      (register-oauth-client
       :client-id +cli-client-id+
       :name "Screenshotbot CLI"
       :public t
       ;; The port is ignored for loopback redirects, see
       ;; REDIRECT-URI-ALLOWED-P.
       :redirect-uris (list "http://127.0.0.1/callback"
                            "http://localhost/callback")
       :scopes (list "profile" "api:read" "api:write"))))))

;; ----------------------------------------------------------------------
;; Grants
;; ----------------------------------------------------------------------

(defindex +grant-user-index+
  'fset-set-index
  :slot-name '%user)

(defindex +grant-company-index+
  'fset-set-index
  :slot-name '%company)

(with-class-validation
  (defclass oauth-grant (store-object)
    ((%client :initarg :client
              :reader grant-client)
     (%user :initarg :user
            :index +grant-user-index+
            :index-reader oauth-grants-for-user
            :reader grant-user)
     (%company :initarg :company
               :index +grant-company-index+
               :index-reader oauth-grants-for-company
               :reader grant-company)
     (%scopes :initarg :scopes
              :initform nil
              :reader grant-scopes)
     (%created-at :initarg :created-at
                  :initform nil
                  :reader grant-created-at)
     (%revoked-p :initarg :revoked-p
                 :initform nil
                 :accessor grant-revoked-p))
    (:metaclass persistent-class)
    (:default-initargs
     :created-at (get-universal-time))
    (:documentation "One user's standing authorization of one client.

Access and refresh tokens both hang off a grant, so revoking the grant
revokes everything that was ever issued from it.")))

(defmethod grant-valid-p ((self oauth-grant))
  (not (grant-revoked-p self)))

(defmethod revoke-grant ((self oauth-grant))
  (with-transaction ()
    (setf (grant-revoked-p self) t)))

;; ----------------------------------------------------------------------
;; Authorization codes
;; ----------------------------------------------------------------------

(defindex +code-index+
  'fset-unique-index
  :slot-name '%code)

(defindex +code-expiry-index+
  'fset-set-index
  :slot-name '%expires-at)

(with-class-validation
  (defclass oauth-code (store-object)
    ((%code :initarg :code
            :index +code-index+
            :index-reader find-oauth-code
            :reader code-string)
     (%grant :initarg :grant
             :reader code-grant)
     (%redirect-uri :initarg :redirect-uri
                    :initform nil
                    :reader code-redirect-uri
                    :documentation "The redirect_uri this code was issued for. RFC 6749 §4.1.3
requires the token request to present the identical value.")
     (%challenge :initarg :challenge
                 :initform nil
                 :reader code-challenge)
     (%challenge-method :initarg :challenge-method
                        :initform nil
                        :reader code-challenge-method)
     (%resource :initarg :resource
                :initform nil
                :reader code-resource
                :documentation "The RFC 8707 resource indicator this code was
authorized for, or NIL. Carried here so the token endpoint can stamp it on
whatever it issues.")
     (%expires-at :initarg :expires-at
                  :index +code-expiry-index+
                  :reader code-expires-at)
     (%consumed-p :initarg :consumed-p
                  :initform nil
                  :accessor code-consumed-p))
    (:metaclass persistent-class)
    (:default-initargs
     :code (random-token 32)
     :expires-at (+ (get-universal-time) *authorization-code-ttl*))))

(defun make-oauth-code (&key grant redirect-uri challenge challenge-method
                          resource)
  (make-instance 'oauth-code
                 :grant grant
                 :redirect-uri redirect-uri
                 :challenge challenge
                 :challenge-method challenge-method
                 :resource resource))

(defvar *code-lock* (bt:make-lock "oauth-code"))

(defun consume-oauth-code (code)
  "Atomically mark CODE as used, returning T if this caller is the one that
consumed it.

RFC 6749 §4.1.2 requires authorization codes to be single use. Losing
this race is a strong signal that the code leaked, and the caller is
expected to revoke the grant."
  (bt:with-lock-held (*code-lock*)
    (cond
      ((code-consumed-p code)
       nil)
      (t
       (with-transaction ()
         (setf (code-consumed-p code) t))
       t))))

;; ----------------------------------------------------------------------
;; Access tokens
;; ----------------------------------------------------------------------

(defindex +access-token-expiry-index+
  'fset-set-index
  :slot-name 'expires-at)

(with-class-validation
  (defclass oauth-access-token (api-key)
    ((%grant :initarg :grant
             :reader access-token-grant)
     (%scopes :initarg :scopes
              :initform nil
              :reader access-token-scopes
              :documentation "May be narrower than the grant's scopes: RFC 6749 §6 lets a
client ask for less than it was given when it refreshes.")
     (%resource :initarg :resource
                :initform nil
                :reader access-token-resource
                :documentation "The audience this token was issued for, per RFC 8707.
NIL means unaudienced: still valid anywhere that doesn't demand an
audience, which is what keeps pre-existing tokens working.")
     (expires-at :initarg :expires-at
                 :index +access-token-expiry-index+
                 :accessor expires-at))
    (:metaclass persistent-class)
    (:default-initargs
     :expires-at (+ (get-universal-time) *access-token-ttl*))
    (:documentation "An OAuth access token.

This is an API-KEY subclass on purpose: it means every existing API
endpoint authorizes an OAuth caller through exactly the same code path
as a hand-created API key, including AUTH:CAN-VIEW! and the
API-KEY-PERMISSIONS checks.")))

(defmethod expired-p ((self oauth-access-token))
  (or
   (< (expires-at self) (get-universal-time))
   (not (grant-valid-p (access-token-grant self)))))

(defmethod api-key-user-visible-p ((self oauth-access-token))
  "Access tokens live for an hour and are minted constantly; listing them
on the API keys page would bury the user's real keys. The grant they
came from is listed instead, on the authorized-applications page."
  nil)

(defun make-access-token (grant &key (scopes (grant-scopes grant)) resource)
  (make-instance 'oauth-access-token
                 :grant grant
                 :scopes scopes
                 :resource resource
                 :user (grant-user grant)
                 :company (grant-company grant)
                 :permissions (scopes-permissions scopes)
                 :description (format nil "OAuth: ~a"
                                      (or (oauth-client-name (grant-client grant))
                                          (oauth-client-id (grant-client grant))))))

(defmethod access-token-string ((self oauth-access-token))
  "The bearer token string handed to the client.

It uses the same encoding as our API tokens, which packs the key id, the
installation hostname and the secret into one string. That is what lets
SCREENSHOTBOT/API/CORE authenticate a bearer token without any
OAuth-specific lookup."
  (encode-api-token self))

(defmethod access-token-expires-in ((self oauth-access-token))
  (max 0 (- (expires-at self) (get-universal-time))))

;; ----------------------------------------------------------------------
;; Refresh tokens
;; ----------------------------------------------------------------------

(defindex +refresh-token-index+
  'fset-unique-index
  :slot-name '%token)

(defindex +refresh-token-expiry-index+
  'fset-set-index
  :slot-name '%expires-at)

(with-class-validation
  (defclass oauth-refresh-token (store-object)
    ((%token :initarg :token
             :index +refresh-token-index+
             :index-reader find-refresh-token
             :reader refresh-token-string)
     (%grant :initarg :grant
             :reader refresh-token-grant)
     (%resource :initarg :resource
                :initform nil
                :reader refresh-token-resource
                :documentation "Carried across rotation so a refreshed token keeps the
audience the original exchange established.")
     (%expires-at :initarg :expires-at
                  :index +refresh-token-expiry-index+
                  :reader refresh-token-expires-at)
     (%revoked-p :initarg :revoked-p
                 :initform nil
                 :accessor refresh-token-revoked-p))
    (:metaclass persistent-class)
    (:default-initargs
     :token (random-token 32)
     :expires-at (+ (get-universal-time) *refresh-token-ttl*))))

(defun make-refresh-token (grant &key resource)
  (make-instance 'oauth-refresh-token :grant grant :resource resource))

(defmethod revoke-refresh-token ((self oauth-refresh-token))
  (with-transaction ()
    (setf (refresh-token-revoked-p self) t)))

;; ----------------------------------------------------------------------
;; Device authorization (RFC 8628)
;; ----------------------------------------------------------------------

(defindex +device-code-index+
  'fset-unique-index
  :slot-name '%device-code)

(defindex +user-code-index+
  'fset-unique-index
  :slot-name '%user-code)

(defindex +device-expiry-index+
  'fset-set-index
  :slot-name '%expires-at)

(defparameter +user-code-alphabet+ "BCDFGHJKLMNPQRSTVWXZ"
  "Consonants only. No vowels means no accidental words, and no 0/O or
1/I/L means nothing to misread off a screen. RFC 8628 §6.1.")

(defun generate-user-code ()
  "An 8 character code the user types into the browser, as XXXX-XXXX."
  (flet ((chunk ()
           (coerce
            (loop repeat 4
                  collect (aref +user-code-alphabet+
                                (secure-random:number (length +user-code-alphabet+))))
            'string)))
    (format nil "~a-~a" (chunk) (chunk))))

(defun normalize-user-code (code)
  "Users retype these by hand, so accept lowercase and a missing dash."
  (let ((cleaned (remove-if-not #'alpha-char-p (string-upcase (or code "")))))
    (if (= 8 (length cleaned))
        (format nil "~a-~a" (subseq cleaned 0 4) (subseq cleaned 4))
        (str:trim (string-upcase (or code ""))))))

(with-class-validation
  (defclass oauth-device-request (store-object)
    ((%device-code :initarg :device-code
                   :index +device-code-index+
                   :index-reader find-device-request
                   :reader device-code-string)
     (%user-code :initarg :user-code
                 :index +user-code-index+
                 :index-reader find-device-request-by-user-code
                 :reader device-user-code)
     (%client :initarg :client
              :reader device-client)
     (%scopes :initarg :scopes
              :initform nil
              :reader device-scopes)
     (%resource :initarg :resource
                :initform nil
                :reader device-resource)
     (%status :initarg :status
              :initform :pending
              :accessor device-status
              :documentation "One of :PENDING, :APPROVED or :DENIED.")
     (%grant :initarg :grant
             :initform nil
             :accessor device-grant)
     (%expires-at :initarg :expires-at
                  :index +device-expiry-index+
                  :reader device-expires-at)
     (%interval :initarg :interval
                :initform nil
                :reader device-interval)
     (%last-polled-at :initarg :last-polled-at
                      :initform nil
                      :accessor device-last-polled-at))
    (:metaclass persistent-class)
    (:default-initargs
     :device-code (random-token 32)
     :user-code (generate-user-code)
     :interval *device-poll-interval*
     :expires-at (+ (get-universal-time) *device-code-ttl*))))

(defun make-device-request (&key client scopes resource)
  (make-instance 'oauth-device-request
                 :client client
                 :scopes scopes
                 :resource resource))

(defmethod approve-device-request ((self oauth-device-request) grant)
  (with-transaction ()
    (setf (device-grant self) grant)
    (setf (device-status self) :approved)))

(defmethod deny-device-request ((self oauth-device-request))
  (with-transaction ()
    (setf (device-status self) :denied)))

(defvar *device-lock* (bt:make-lock "oauth-device"))

(defmethod consume-device-request ((self oauth-device-request))
  "Atomically claim an approved device request, returning its grant.

A device code may only be exchanged once (RFC 8628 §3.5), so a second
caller -- or a racing second poll -- gets NIL."
  (bt:with-lock-held (*device-lock*)
    (when (eql :approved (device-status self))
      (with-transaction ()
        (setf (device-status self) :consumed))
      (device-grant self))))

(defmethod note-device-poll ((self oauth-device-request))
  "Record a poll, returning T if the client polled faster than it was told to.

RFC 8628 §3.5 `slow_down`."
  (let ((now (get-universal-time))
        (last (device-last-polled-at self)))
    (with-transaction ()
      (setf (device-last-polled-at self) now))
    (and last (< (- now last) (device-interval self)))))

;; ----------------------------------------------------------------------
;; Expiry
;; ----------------------------------------------------------------------

(defun %delete-expired (index expires-at-fn)
  "Delete every object in INDEX whose expiry is in the past.

INDEX is ordered by expiry, so we only ever look at the head."
  (let ((now (get-universal-time)))
    (loop for next = (index-least index)
          while (and next (< (funcall expires-at-fn next) now))
          do (delete-object next))))

(defun cleanup-expired-oauth-objects ()
  (%delete-expired +code-expiry-index+ #'code-expires-at)
  (%delete-expired +device-expiry-index+ #'device-expires-at)
  (%delete-expired +access-token-expiry-index+ #'expires-at)
  (%delete-expired +refresh-token-expiry-index+ #'refresh-token-expires-at))

(def-cron cleanup-expired-oauth-objects (:step-min 5)
  (cleanup-expired-oauth-objects))

