;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth-server/client/test-pkce
  (:use #:cl
        #:fiveam)
  (:import-from #:auth-server/client/pkce
                #:base64url-encode
                #:code-challenge
                #:make-code-verifier
                #:random-token))
(in-package :auth-server/client/test-pkce)

(util/fiveam:def-suite)

(test code-challenge-matches-the-rfc-7636-appendix-b-vector
  "The one test that proves this client can talk to any conforming server,
and that it agrees with our own SCREENSHOTBOT/AUTH-SERVER/PKCE."
  (is (equal "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
             (code-challenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"))))

(test verifiers-satisfy-the-rfc-7636-format-rules
  (loop repeat 50
        for verifier = (make-code-verifier)
        do
           ;; §4.1: 43-128 characters from an unreserved alphabet.
           (is (<= 43 (length verifier) 128))
           (is-true (every (lambda (ch)
                             (or (alphanumericp ch)
                                 (find ch "-._~")))
                           verifier))))

(test verifiers-are-not-repeated
  (let ((verifiers (loop repeat 200 collect (make-code-verifier))))
    (is (= 200 (length (remove-duplicates verifiers :test #'equal))))))

(test base64url-emits-no-padding-or-unsafe-characters
  (loop for length from 1 to 40
        for encoded = (base64url-encode
                       (make-array length :element-type '(unsigned-byte 8)
                                          :initial-element 251))
        do
           (is-false (str:containsp "=" encoded))
           (is-false (str:containsp "+" encoded))
           (is-false (str:containsp "/" encoded))))

(test random-token-length-tracks-the-entropy-asked-for
  (is (= 43 (length (random-token 32))))
  (is (= 22 (length (random-token 16)))))

(test challenge-is-deterministic-for-a-given-verifier
  (let ((verifier (make-code-verifier)))
    (is (equal (code-challenge verifier) (code-challenge verifier)))
    (is-false (equal (code-challenge verifier)
                     (code-challenge (make-code-verifier))))))
