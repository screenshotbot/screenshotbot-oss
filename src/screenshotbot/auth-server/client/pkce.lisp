;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/pkce
  (:use #:cl)
  (:export
   #:base64url-encode
   #:random-token
   #:make-code-verifier
   #:code-challenge
   #:+code-challenge-method+)
  (:documentation "The client half of PKCE, RFC 7636.

Only generation lives here; verification is the authorization server's
job. That asymmetry is why this doesn't share code with
SCREENSHOTBOT/AUTH-SERVER/PKCE -- the overlap is two lines, and sharing
them would mean this system depending on the whole server."))
(in-package :auth-server/client/pkce)

(defparameter +code-challenge-method+ "S256"
  "RFC 8252 §8.1 requires S256 from any client that can compute it, and
the authorization server refuses `plain` outright.")

(defun base64url-encode (bytes)
  "base64url without padding, per RFC 7636 §4.2."
  (let ((encoded (base64:usb8-array-to-base64-string bytes)))
    (str:replace-all
     "=" ""
     (str:replace-all
      "/" "_"
      (str:replace-all "+" "-" encoded)))))

(defun random-token (&optional (num-bytes 32))
  "A URL-safe token carrying NUM-BYTES of cryptographic entropy."
  (base64url-encode (secure-random:bytes num-bytes secure-random:*generator*)))

(defun make-code-verifier ()
  "A fresh code verifier.

32 random bytes base64url-encode to 43 characters, which is exactly the
minimum RFC 7636 §4.1 allows and well past the 256 bits of entropy it
asks for."
  (random-token 32))

(defun code-challenge (verifier)
  "BASE64URL(SHA256(ASCII(VERIFIER))), per RFC 7636 §4.2."
  (base64url-encode
   (ironclad:digest-sequence
    :sha256
    (flexi-streams:string-to-octets verifier :external-format :utf-8))))
