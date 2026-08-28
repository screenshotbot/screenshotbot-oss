;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/cli
  (:use #:cl)
  (:import-from #:auth-server/client
                #:+default-client-id+
                #:access-token
                #:call-api
                #:client-error
                #:oauth-client
                #:sign-out)
  (:import-from #:clingon.command
                #:getopt)
  (:import-from #:clingon.options
                #:make-option)
  (:export
   #:main
   #:root/command)
  (:documentation "Command line front end for :auth-server.client.

Exercises the Screenshotbot OAuth authorization server end to end: sign
in the way a CLI has to, then spend the resulting token on a real API
call, because a token you can't use proves nothing."))
(in-package :auth-server/client/cli)

(defun common-options ()
  (list
   (make-option
    :string
    :long-name "host"
    :initial-value "https://screenshotbot.io"
    :description "Base URL of the installation"
    :key :host)
   (make-option
    :string
    :long-name "client-id"
    :initial-value +default-client-id+
    :description "OAuth client_id"
    :key :client-id)
   (make-option
    :string
    :long-name "scope"
    :initial-value "api:read"
    :description "Space separated scopes to request"
    :key :scope)
   (make-option
    :flag
    :long-name "device"
    :initial-value nil
    :description "Use the device flow (RFC 8628) instead of a loopback redirect"
    :key :device)
   (make-option
    :flag
    :long-name "no-browser"
    :initial-value nil
    :description "Print the authorization URL instead of opening it"
    :key :no-browser)
   (make-option
    :flag
    :long-name "no-discovery"
    :initial-value nil
    :description "Skip RFC 8414 discovery and assume the standard paths"
    :key :no-discovery)
   (make-option
    :flag
    :long-name "fresh"
    :initial-value nil
    :description "Ignore any cached token and sign in again"
    :key :fresh)
   (make-option
    :string
    :long-name "token-file"
    :initial-value nil
    :description "Where to cache tokens (default: ~/.config/screenshotbot/oauth-token.json)"
    :key :token-file)
   (make-option
    :integer
    :long-name "timeout"
    :initial-value 300
    :description "Seconds to wait for the user to authorize"
    :key :timeout)))

(defun client-from-options (cmd)
  (make-instance 'oauth-client
                 :host (getopt cmd :host)
                 :client-id (getopt cmd :client-id)
                 :scope (getopt cmd :scope)
                 :flow (if (getopt cmd :device) :device :browser)
                 :open-browser-p (not (getopt cmd :no-browser))
                 :use-discovery-p (not (getopt cmd :no-discovery))
                 :timeout (getopt cmd :timeout)
                 :token-file (let ((file (getopt cmd :token-file)))
                               (unless (str:emptyp file)
                                 (pathname file)))))

(defun print-json (value)
  (let ((json:*json-output* *standard-output*))
    (json:encode-json value))
  (terpri))

;; ----------------------------------------------------------------------
;; fetch-run
;; ----------------------------------------------------------------------

(defun fetch-run/handler (cmd)
  (let ((run-id (first (clingon:command-arguments cmd))))
    (when (str:emptyp run-id)
      (format *error-output* "~&Give me a run id to fetch.~%")
      (clingon:exit 2))
    (let* ((client (client-from-options cmd))
           (response (call-api client
                               (format nil "/api/run/~a" (quri:url-encode run-id))
                               :force-new (getopt cmd :fresh))))
      (print-json response)
      ;; The API reports failures in the body with a 200, so the exit code
      ;; has to come from the payload or a caller can't tell.
      (when (eq :false (alexandria:assoc-value response "success" :test #'equal))
        (clingon:exit 1)))))

(defun fetch-run/command ()
  (clingon:make-command
   :name "fetch-run"
   :description "Sign in over OAuth and fetch a run as JSON"
   :usage "<RUN-ID>"
   :options (common-options)
   :handler #'fetch-run/handler))

;; ----------------------------------------------------------------------
;; token / logout
;; ----------------------------------------------------------------------

(defun token/command ()
  (clingon:make-command
   :name "token"
   :description "Print an access token, signing in if necessary"
   :options (common-options)
   :handler (lambda (cmd)
              (format t "~a~%" (access-token (client-from-options cmd)
                                             :force-new (getopt cmd :fresh))))))

(defun logout/command ()
  (clingon:make-command
   :name "logout"
   :description "Revoke the cached grant and delete the local token"
   :options (common-options)
   :handler (lambda (cmd)
              (if (sign-out (client-from-options cmd))
                  (format t "~&Signed out.~%")
                  (format t "~&Nothing cached for that host.~%")))))

;; ----------------------------------------------------------------------

(defun root/command ()
  (clingon:make-command
   :name "test-oauth-flow"
   :description "Exercise the Screenshotbot OAuth authorization server"
   :sub-commands (list (fetch-run/command)
                       (token/command)
                       (logout/command))
   :handler (lambda (cmd) (clingon:print-usage-and-exit cmd t))))

(defun main (argv)
  "Entry point. ARGV must not include the program name."
  (handler-case
      (clingon:run (root/command) argv)
    (client-error (e)
      ;; These are expected failures -- declined, timed out, bad scope --
      ;; and a backtrace would only bury the one line that matters.
      (format *error-output* "~&~a~%" e)
      (clingon:exit 1))))
