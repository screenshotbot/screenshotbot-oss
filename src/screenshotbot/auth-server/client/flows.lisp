;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/flows
  (:use #:cl)
  (:import-from #:auth-server/client/conditions
                #:authorization-declined
                #:authorization-timeout
                #:oauth-error
                #:oauth-error-code
                #:state-mismatch)
  (:import-from #:auth-server/client/discovery
                #:authorization-endpoint
                #:device-authorization-endpoint
                #:revocation-endpoint
                #:token-endpoint)
  (:import-from #:auth-server/client/http
                #:field
                #:post-form)
  (:import-from #:auth-server/client/loopback
                #:await-callback
                #:listener-redirect-uri
                #:with-loopback-listener)
  (:import-from #:auth-server/client/pkce
                #:+code-challenge-method+
                #:code-challenge
                #:make-code-verifier
                #:random-token)
  (:export
   #:+device-grant-type+
   #:authorization-code-flow
   #:device-flow
   #:refresh-tokens
   #:revoke-token
   #:open-in-browser
   #:*announce*))
(in-package :auth-server/client/flows)

(defparameter +device-grant-type+
  "urn:ietf:params:oauth:grant-type:device_code"
  "RFC 8628 §3.4.")

(defvar *announce*
  (lambda (control &rest args)
    (apply #'format *standard-output* control args)
    (finish-output *standard-output*))
  "How the flows talk to the user. Rebindable so a caller with its own UI
-- or a test -- isn't forced to write to stdout.")

(defun announce (control &rest args)
  (apply *announce* control args))

(defun %sleep (seconds)
  "Indirection so tests can drive the device-poll loop without waiting on
a real clock."
  (sleep seconds))

(defun open-in-browser (url)
  "Best-effort launch of the user's browser. Returns whether we managed to
start something, which is *not* the same as the page having opened."
  (handler-case
      (progn
        (uiop:launch-program
         (cond
           ((uiop:os-macosx-p) (list "open" url))
           ((uiop:os-windows-p) (list "cmd" "/c" "start" "" url))
           (t (list "xdg-open" url)))
         :output nil :error-output nil)
        t)
    (error () nil)))

;; ----------------------------------------------------------------------
;; Authorization code with PKCE, over a loopback redirect
;; ----------------------------------------------------------------------

(defun %authorization-url (metadata &key client-id scope state challenge redirect-uri)
  (let ((uri (quri:uri (authorization-endpoint metadata))))
    (setf (quri:uri-query-params uri)
          (append (quri:uri-query-params uri)
                  `(("response_type" . "code")
                    ("client_id" . ,client-id)
                    ("redirect_uri" . ,redirect-uri)
                    ("scope" . ,scope)
                    ("state" . ,state)
                    ("code_challenge" . ,challenge)
                    ("code_challenge_method" . ,+code-challenge-method+))))
    (quri:render-uri uri)))

(defun authorization-code-flow (metadata &key client-id scope
                                           (open-browser t) (timeout 300))
  "Sign in through a browser on this machine. Returns the token response."
  (with-loopback-listener (listener)
    (let* ((redirect-uri (listener-redirect-uri listener))
           (verifier (make-code-verifier))
           (state (random-token 16))
           (url (%authorization-url metadata
                                    :client-id client-id
                                    :scope scope
                                    :state state
                                    :challenge (code-challenge verifier)
                                    :redirect-uri redirect-uri)))
      ;; Always print it. Launching a browser fails silently often enough
      ;; that the URL on screen is the only reliable path.
      (announce "~&Open this URL to authorize:~2%    ~a~2%" url)
      (when open-browser
        (open-in-browser url))

      (let* ((params (await-callback listener :timeout timeout))
             (returned-state (field params "state")))
        (let ((error-code (field params "error")))
          (when error-code
            (if (equal "access_denied" error-code)
                (error 'authorization-declined
                       :description (field params "error_description"))
                (error 'oauth-error
                       :code error-code
                       :description (field params "error_description")))))
        ;; Anything on this machine could hit our port. Without this check
        ;; an attacker could feed us their own code and bind our session to
        ;; their account -- RFC 6749 §10.12.
        (unless (equal returned-state state)
          (error 'state-mismatch))
        (unless (field params "code")
          (error 'oauth-error :code "invalid_request"
                              :description "The redirect carried no code"))

        (post-form (token-endpoint metadata)
                   `(("grant_type" . "authorization_code")
                     ("client_id" . ,client-id)
                     ("code" . ,(field params "code"))
                     ("redirect_uri" . ,redirect-uri)
                     ("code_verifier" . ,verifier)))))))

;; ----------------------------------------------------------------------
;; Device authorization, RFC 8628
;; ----------------------------------------------------------------------

(defun device-flow (metadata &key client-id scope (open-browser t) (timeout 300))
  "Sign in with a code the user types into a browser anywhere."
  (let* ((authorization
           (post-form (device-authorization-endpoint metadata)
                      `(("client_id" . ,client-id)
                        ("scope" . ,scope))))
         (complete (field authorization "verification_uri_complete")))
    (announce "~&~%  To sign in, visit:  ~a~%  and enter the code: ~a~%"
              (field authorization "verification_uri")
              (field authorization "user_code"))
    (when complete
      (announce "~%  (or open directly: ~a)~%" complete)
      (when open-browser
        (open-in-browser complete)))
    (announce "~%")

    (%poll-for-device-token metadata
                            :client-id client-id
                            :device-code (field authorization "device_code")
                            :interval (or (field authorization "interval") 5)
                            :timeout (min (or (field authorization "expires_in") 900)
                                          timeout))))

(defun %poll-for-device-token (metadata &key client-id device-code interval timeout)
  (let ((deadline (+ (get-universal-time) timeout))
        (interval interval))
    (loop
      (when (> (get-universal-time) deadline)
        (error 'authorization-timeout :seconds timeout))
      (%sleep interval)
      (handler-case
          (return
            (post-form (token-endpoint metadata)
                       `(("grant_type" . ,+device-grant-type+)
                         ("client_id" . ,client-id)
                         ("device_code" . ,device-code))))
        (oauth-error (e)
          (let ((code (oauth-error-code e)))
            (cond
              ((equal "authorization_pending" code))
              ;; RFC 8628 §3.5: back off by five seconds and keep going.
              ((equal "slow_down" code)
               (incf interval 5))
              ((equal "access_denied" code)
               (error 'authorization-declined))
              (t
               (error e)))))))))

;; ----------------------------------------------------------------------
;; Refresh and revocation
;; ----------------------------------------------------------------------

(defun refresh-tokens (metadata &key client-id refresh-token scope)
  (post-form (token-endpoint metadata)
             `(("grant_type" . "refresh_token")
               ("client_id" . ,client-id)
               ("refresh_token" . ,refresh-token)
               ("scope" . ,scope))))

(defun revoke-token (metadata &key client-id token)
  "RFC 7009. The server answers 200 with an empty body whether or not it
recognised the token, so that this endpoint can't be used to probe which
tokens exist."
  (post-form (revocation-endpoint metadata)
             `(("client_id" . ,client-id)
               ("token" . ,token))
             :allow-empty t)
  t)
