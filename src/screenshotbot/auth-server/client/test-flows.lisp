;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/test-flows
  (:use #:cl
        #:fiveam)
  (:import-from #:auth-server/client/conditions
                #:authorization-declined
                #:authorization-timeout
                #:oauth-error
                #:oauth-error-code
                #:state-mismatch)
  (:import-from #:auth-server/client/discovery
                #:conventional-metadata)
  (:import-from #:auth-server/client/flows
                #:*announce*
                #:%poll-for-device-token
                #:%sleep
                #:authorization-code-flow
                #:device-flow
                #:open-in-browser
                #:refresh-tokens
                #:revoke-token)
  (:import-from #:auth-server/client/http
                #:post-form)
  (:import-from #:auth-server/client/loopback
                #:await-callback
                #:loopback-listener
                #:start-listener
                #:stop-listener)
  (:import-from #:auth-server/client/pkce
                #:code-challenge)
  (:import-from #:cl-mock
                #:if-called
                #:with-mocks)
  (:local-nicknames (#:a #:alexandria))
  (:documentation "Tests for the OAuth flows, with the network and the clock
stubbed out.

These deliberately don't open a socket: what's interesting here is the
decision logic -- when to retry, when to back off, when to refuse a
redirect -- and that is all deterministic once POST-FORM and %SLEEP are
under our control."))
(in-package :auth-server/client/test-flows)

(util/fiveam:def-suite)

(defparameter +token-response+
  '(("access_token" . "the-access-token")
    ("refresh_token" . "the-refresh-token")
    ("token_type" . "Bearer")
    ("expires_in" . 3600)
    ("scope" . "api:read")))

(defun oauth-failure (code &optional description)
  "A thunk that fails the way a server's error response would."
  (lambda ()
    (error 'oauth-error :code code :description description)))

(def-fixture state ()
  (with-mocks ()
    (let* ((metadata (conventional-metadata "https://example.com"))
           (posts nil)      ; every (url . params) we sent, oldest first
           (sleeps nil)     ; every interval we were asked to wait
           (announced nil)
           (*announce* (lambda (control &rest args)
                         (push (apply #'format nil control args) announced))))
      (declare (ignorable metadata))
      (if-called '%sleep (lambda (seconds) (push seconds sleeps)))
      (if-called 'open-in-browser (lambda (url) (declare (ignore url)) t))
      (labels ((sent ()
                 (reverse posts))
               (waits ()
                 (reverse sleeps))
               (said ()
                 (format nil "~{~a~}" (reverse announced)))
               (respond-with (&rest thunks)
                 "Answer successive POST-FORMs from THUNKS, repeating the last."
                 (let ((remaining thunks))
                   (if-called 'post-form
                              (lambda (url params &key allow-empty)
                                (declare (ignore allow-empty))
                                (push (cons url params) posts)
                                (let ((thunk (if (cdr remaining)
                                                 (pop remaining)
                                                 (car remaining))))
                                  (funcall thunk))))))
               (last-params ()
                 (cdr (car posts)))
               (param (name &optional (params (last-params)))
                 (a:assoc-value params name :test #'equal)))
        (declare (ignorable #'sent #'waits #'said #'respond-with
                            #'last-params #'param))
        (&body)))))

;; ----------------------------------------------------------------------
;; Authorization code flow
;; ----------------------------------------------------------------------

(defun stub-listener ()
  "A listener that was never started, so it holds no socket.

LISTENER-REDIRECT-URI still works off it, and reports port 0, which is
all the flow needs."
  (make-instance 'loopback-listener))

(def-fixture browser-returns (params)
  "Run the authorization code flow against a canned browser redirect."
  (if-called 'start-listener (lambda (&key path) (declare (ignore path))
                               (stub-listener)))
  (if-called 'stop-listener (lambda (listener) (declare (ignore listener)) nil))
  (if-called 'await-callback (lambda (listener &key timeout)
                               (declare (ignore listener timeout))
                               params))
  (&body))

(defun url-param (text name)
  "Pull NAME's value out of the query string embedded in TEXT.

The flow prints the authorization URL before it waits, so this is how a
test reads back the values it generated -- the state and the challenge
are chosen inside the flow, not handed to it."
  (let ((marker (format nil "~a=" name)))
    (a:when-let ((start (search marker text)))
      (let* ((rest (subseq text (+ start (length marker))))
             (end (position #\& rest)))
        (quri:url-decode (subseq rest 0 (or end (length rest))))))))

(test the-code-verifier-sent-matches-the-challenge-advertised
  "The end-to-end PKCE property, proved without a server: the verifier we
reveal at the token endpoint must hash to the challenge we published in
the authorization URL."
  (with-fixture state ()
    (if-called 'start-listener (lambda (&key path) (declare (ignore path))
                                 (stub-listener)))
    (if-called 'stop-listener (lambda (l) (declare (ignore l)) nil))
    ;; Echo back the state the flow chose, so it accepts its own redirect.
    (if-called 'await-callback
               (lambda (l &key timeout)
                 (declare (ignore l timeout))
                 `(("code" . "the-code")
                   ("state" . ,(url-param (said) "state")))))
    (respond-with (lambda () +token-response+))
    (authorization-code-flow metadata :client-id "cli" :scope "api:read")
    (let ((challenge (url-param (said) "code_challenge"))
          (verifier (param "code_verifier")))
      (is-true challenge)
      (is-true verifier)
      (is (equal challenge (code-challenge verifier)))
      ;; And the rest of the exchange is what RFC 6749 §4.1.3 asks for.
      (is (equal "authorization_code" (param "grant_type")))
      (is (equal "the-code" (param "code")))
      (is (equal (url-param (said) "redirect_uri") (param "redirect_uri"))))))

(test authorization-code-flow-refuses-a-mismatched-state
  "Anything on this machine can hit our loopback port; without this check
an attacker could bind our session to their account."
  (with-fixture state ()
    (with-fixture browser-returns ('(("code" . "the-code") ("state" . "not-ours")))
      (respond-with (lambda () +token-response+))
      (signals state-mismatch
        (authorization-code-flow metadata :client-id "cli" :scope "api:read"))
      ;; And crucially, we never sent the code anywhere.
      (is (equal nil (sent))))))

(test authorization-code-flow-reports-a-declined-authorization
  (with-fixture state ()
    (with-fixture browser-returns ('(("error" . "access_denied")
                                     ("error_description" . "user said no")))
      (signals authorization-declined
        (authorization-code-flow metadata :client-id "cli" :scope "api:read"))
      (is (equal nil (sent))))))

(test authorization-code-flow-passes-other-errors-through
  (with-fixture state ()
    (with-fixture browser-returns ('(("error" . "invalid_scope")
                                     ("error_description" . "nope")))
      (let ((e (handler-case
                   (progn (authorization-code-flow metadata :client-id "cli"
                                                            :scope "bad")
                          nil)
                 (oauth-error (e) e))))
        (is-true e)
        (is (equal "invalid_scope" (oauth-error-code e)))))))

(test authorization-code-flow-rejects-a-redirect-with-no-code
  "The state has to be echoed correctly for this to be reached at all --
state is validated first, so an unrecognised redirect is rejected before
we look at anything it claims to carry."
  (with-fixture state ()
    (if-called 'start-listener (lambda (&key path) (declare (ignore path))
                                 (stub-listener)))
    (if-called 'stop-listener (lambda (l) (declare (ignore l)) nil))
    (if-called 'await-callback
               (lambda (l &key timeout)
                 (declare (ignore l timeout))
                 `(("state" . ,(url-param (said) "state")))))
    (signals oauth-error
      (authorization-code-flow metadata :client-id "cli" :scope "api:read"))
    ;; Nothing was sent to the token endpoint.
    (is (equal nil (sent)))))

(test the-authorization-url-is-always-printed-even-when-a-browser-opens
  "Launching a browser fails silently often enough that the URL on screen
is the only reliable path."
  (with-fixture state ()
    (with-fixture browser-returns ('(("error" . "access_denied")))
      (ignore-errors
       (authorization-code-flow metadata :client-id "cli" :scope "api:read"
                                         :open-browser t))
      (is-true (str:containsp "https://example.com/oauth/authorize" (said)))
      (is-true (str:containsp "code_challenge_method=S256" (said)))
      (is-true (str:containsp "response_type=code" (said))))))

;; ----------------------------------------------------------------------
;; Device flow polling
;; ----------------------------------------------------------------------

(test polling-retries-while-the-user-has-not-answered
  (with-fixture state ()
    (respond-with (oauth-failure "authorization_pending")
                  (oauth-failure "authorization_pending")
                  (lambda () +token-response+))
    (let ((response (%poll-for-device-token metadata
                                            :client-id "cli"
                                            :device-code "dc"
                                            :interval 5
                                            :timeout 600)))
      (is (equal "the-access-token" (a:assoc-value response "access_token"
                                                   :test #'equal)))
      (is (equal 3 (length (sent)))))))

(test slow-down-widens-the-interval-by-five-seconds
  "RFC 8628 §3.5."
  (with-fixture state ()
    (respond-with (oauth-failure "slow_down")
                  (oauth-failure "slow_down")
                  (lambda () +token-response+))
    (%poll-for-device-token metadata :client-id "cli" :device-code "dc"
                                     :interval 5 :timeout 600)
    ;; First wait at the interval we were given, then widened twice.
    (is (equal '(5 10 15) (waits)))))

(test polling-stops-when-the-user-declines
  (with-fixture state ()
    (respond-with (oauth-failure "access_denied"))
    (signals authorization-declined
      (%poll-for-device-token metadata :client-id "cli" :device-code "dc"
                                       :interval 5 :timeout 600))
    (is (equal 1 (length (sent))))))

(test polling-does-not-swallow-an-unexpected-error
  "A revoked or unknown device code must surface, not spin until timeout."
  (with-fixture state ()
    (respond-with (oauth-failure "invalid_grant" "gone"))
    (let ((e (handler-case
                 (progn (%poll-for-device-token metadata :client-id "cli"
                                                         :device-code "dc"
                                                         :interval 5 :timeout 600)
                        nil)
               (oauth-error (e) e))))
      (is-true e)
      (is (equal "invalid_grant" (oauth-error-code e))))))

(test polling-gives-up-at-the-deadline
  (with-fixture state ()
    (respond-with (oauth-failure "authorization_pending"))
    (signals authorization-timeout
      (%poll-for-device-token metadata :client-id "cli" :device-code "dc"
                                       :interval 5 :timeout -1))))

(test the-device-flow-shows-the-user-the-code-and-the-url
  (with-fixture state ()
    (respond-with (lambda ()
                    '(("device_code" . "dc")
                      ("user_code" . "GJTK-BWDF")
                      ("verification_uri" . "https://example.com/oauth/device")
                      ("verification_uri_complete"
                       . "https://example.com/oauth/device?user_code=GJTK-BWDF")
                      ("expires_in" . 900)
                      ("interval" . 5)))
                  (lambda () +token-response+))
    (device-flow metadata :client-id "cli" :scope "api:read")
    (is-true (str:containsp "GJTK-BWDF" (said)))
    (is-true (str:containsp "https://example.com/oauth/device" (said)))
    ;; The device authorization request, then one poll.
    (is (equal 2 (length (sent))))
    (is (equal "https://example.com/oauth/device/code" (car (first (sent)))))
    (is (equal "https://example.com/oauth/token" (car (second (sent)))))))

;; ----------------------------------------------------------------------
;; Refresh and revoke
;; ----------------------------------------------------------------------

(test refresh-sends-the-grant-type-and-token-the-rfc-requires
  (with-fixture state ()
    (respond-with (lambda () +token-response+))
    (refresh-tokens metadata :client-id "cli" :refresh-token "rt")
    (is (equal "https://example.com/oauth/token" (car (car posts))))
    (is (equal "refresh_token" (param "grant_type")))
    (is (equal "rt" (param "refresh_token")))
    (is (equal "cli" (param "client_id")))
    ;; Absent rather than empty: an empty scope is not the same as no scope.
    (is-false (param "scope"))))

(test refresh-can-narrow-the-scope
  (with-fixture state ()
    (respond-with (lambda () +token-response+))
    (refresh-tokens metadata :client-id "cli" :refresh-token "rt" :scope "api:read")
    (is (equal "api:read" (param "scope")))))

(test revoke-posts-to-the-revocation-endpoint
  (with-fixture state ()
    (respond-with (lambda () nil))
    (is-true (revoke-token metadata :client-id "cli" :token "rt"))
    (is (equal "https://example.com/oauth/revoke" (car (car posts))))
    (is (equal "rt" (param "token")))))
