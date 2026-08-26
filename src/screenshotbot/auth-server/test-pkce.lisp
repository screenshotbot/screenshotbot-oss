;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-pkce
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/auth-server/pkce
                #:base64url-encode
                #:constant-time-equal
                #:random-token
                #:s256-challenge
                #:valid-code-challenge-method-p
                #:valid-code-verifier-p
                #:verify-code-verifier))
(in-package :screenshotbot/auth-server/test-pkce)

(util/fiveam:def-suite)

(test s256-matches-the-rfc-7636-appendix-b-vector
  ;; RFC 7636 Appendix B gives this verifier and the challenge it must
  ;; produce. If this ever fails, no standard client can talk to us.
  (is (equal "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
             (s256-challenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"))))

(test base64url-has-no-padding-or-unsafe-characters
  (loop for length from 1 to 40
        for encoded = (base64url-encode
                       (make-array length :element-type '(unsigned-byte 8)
                                          :initial-element 251))
        do
           (is-false (str:containsp "=" encoded))
           (is-false (str:containsp "+" encoded))
           (is-false (str:containsp "/" encoded))))

(test random-tokens-are-unique-and-long-enough
  (let ((tokens (loop repeat 100 collect (random-token 32))))
    (is (= 100 (length (remove-duplicates tokens :test #'equal))))
    (is (every (lambda (token) (>= (length token) 43)) tokens))))

(test constant-time-equal
  (is-true (constant-time-equal "abcdef" "abcdef"))
  (is-false (constant-time-equal "abcdef" "abcdeg"))
  (is-false (constant-time-equal "abcdef" "abcde"))
  (is-false (constant-time-equal "abcdef" nil))
  (is-false (constant-time-equal nil "abcdef"))
  (is-false (constant-time-equal nil nil)))

(test valid-code-verifier-p-enforces-rfc-7636-length-and-charset
  (is-true (valid-code-verifier-p (make-string 43 :initial-element #\a)))
  (is-true (valid-code-verifier-p (make-string 128 :initial-element #\a)))
  (is-true (valid-code-verifier-p
            (format nil "~a-._~~" (make-string 43 :initial-element #\a))))
  ;; too short
  (is-false (valid-code-verifier-p (make-string 42 :initial-element #\a)))
  ;; too long
  (is-false (valid-code-verifier-p (make-string 129 :initial-element #\a)))
  ;; illegal character
  (is-false (valid-code-verifier-p
             (format nil "~a/" (make-string 43 :initial-element #\a))))
  (is-false (valid-code-verifier-p nil)))

(test only-s256-is-accepted
  (is-true (valid-code-challenge-method-p "S256"))
  ;; RFC 8252 §8.1: we don't let a client downgrade to plain.
  (is-false (valid-code-challenge-method-p "plain"))
  (is-false (valid-code-challenge-method-p "s256"))
  (is-false (valid-code-challenge-method-p nil)))

(test verify-code-verifier-round-trip
  (let* ((verifier (random-token 32))
         (challenge (s256-challenge verifier)))
    (is-true (verify-code-verifier :code-challenge challenge
                                   :code-challenge-method "S256"
                                   :code-verifier verifier))
    (is-false (verify-code-verifier :code-challenge challenge
                                    :code-challenge-method "S256"
                                    :code-verifier (random-token 32)))
    ;; A verifier that doesn't even meet the format requirements can
    ;; never pass, whatever the challenge says.
    (is-false (verify-code-verifier :code-challenge challenge
                                    :code-challenge-method "S256"
                                    :code-verifier "short"))
    (is-false (verify-code-verifier :code-challenge challenge
                                    :code-challenge-method "unknown"
                                    :code-verifier verifier))
    (is-false (verify-code-verifier :code-challenge challenge
                                    :code-challenge-method nil
                                    :code-verifier verifier))))

(test plain-verifier-is-not-silently-accepted-against-an-s256-challenge
  "The classic PKCE downgrade: offer the challenge back as the verifier."
  (let* ((verifier (random-token 32))
         (challenge (s256-challenge verifier)))
    (is-false (verify-code-verifier :code-challenge challenge
                                    :code-challenge-method "S256"
                                    :code-verifier challenge))))
