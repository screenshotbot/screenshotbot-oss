;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/pkce
  (:use #:cl)
  (:export
   #:base64url-encode
   #:random-token
   #:constant-time-equal
   #:s256-challenge
   #:valid-code-verifier-p
   #:valid-code-challenge-method-p
   #:verify-code-verifier
   #:*supported-code-challenge-methods*)
  (:documentation "Proof Key for Code Exchange, RFC 7636.

Without PKCE a public client (which by definition can't keep a secret)
is vulnerable to anyone who can observe the authorization code -- and on
a CLI that's every other process on the machine, since the code arrives
on a loopback redirect. See RFC 8252 §8.1."))
(in-package :screenshotbot/auth-server/pkce)

(defparameter *supported-code-challenge-methods*
  '("S256")
  "RFC 7636 also defines `plain`, but RFC 8252 §8.1 requires S256 whenever
the client can compute it, and every client we care about can.")

(defun base64url-encode (bytes)
  "base64url without padding, as required by RFC 7636 §4.2."
  (let ((encoded (base64:usb8-array-to-base64-string bytes)))
    (str:replace-all
     "=" ""
     (str:replace-all
      "/" "_"
      (str:replace-all "+" "-" encoded)))))

(defun random-token (&optional (num-bytes 32))
  "A cryptographically random, URL safe token of NUM-BYTES of entropy."
  (base64url-encode (secure-random:bytes num-bytes secure-random:*generator*)))

(defun constant-time-equal (one two)
  "Compare two strings without leaking their contents through timing.

Length is not secret here (all our tokens are fixed length), but the
contents are."
  (and
   (stringp one)
   (stringp two)
   (= (length one) (length two))
   (let ((diff 0))
     (loop for a across one
           for b across two
           do (setf diff (logior diff (logxor (char-code a) (char-code b)))))
     (= 0 diff))))

(defun s256-challenge (verifier)
  "BASE64URL(SHA256(ASCII(VERIFIER))), per RFC 7636 §4.2."
  (base64url-encode
   (ironclad:digest-sequence
    :sha256
    (flexi-streams:string-to-octets verifier :external-format :utf-8))))

(defun valid-code-verifier-p (verifier)
  "RFC 7636 §4.1: 43-128 characters of [A-Za-z0-9-._~]."
  (and
   (stringp verifier)
   (<= 43 (length verifier) 128)
   (every (lambda (ch)
            (or (alphanumericp ch)
                (member ch '(#\- #\. #\_ #\~))))
          verifier)))

(defun valid-code-challenge-method-p (method)
  (member method *supported-code-challenge-methods* :test #'equal))

(defun verify-code-verifier (&key code-challenge code-challenge-method code-verifier)
  "Does CODE-VERIFIER match the CODE-CHALLENGE that was registered at
authorization time? Returns a boolean."
  (and
   (valid-code-verifier-p code-verifier)
   (cond
     ((equal "S256" code-challenge-method)
      (constant-time-equal code-challenge (s256-challenge code-verifier)))
     ((equal "plain" code-challenge-method)
      ;; Only reachable if an installation adds "plain" to
      ;; *supported-code-challenge-methods*.
      (constant-time-equal code-challenge code-verifier))
     (t
      nil))))
