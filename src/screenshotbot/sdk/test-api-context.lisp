;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-api-context
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/api-context
                #:extract-hostname-from-secret
                #:fetch-remote-information
                #:base-api-context
                #:api-feature-enabled-p
                #:hostname
                #:key
                #:secret
                #:api-context)
  (:import-from #:core/api/model/api-key
                #:make-encoded-secret))
(in-package :screenshotbot/sdk/test-api-context)

(util/fiveam:def-suite)

(def-fixture state ()
  (&body))

(defclass test-api-context (base-api-context)
  ())

(defmethod fetch-remote-information ((self test-api-context))
  (values))

(test default-api-hostname

  (is
   (equal "https://api.screenshotbot.io"
          (hostname
           (make-instance 'api-context
                          :hostname nil))))
  (is
   (equal "https://api.screenshotbot.io"
          (hostname
           (make-instance 'api-context
                          :hostname ""))))  
  (is
   (equal "https://api.screenshotbot.io"
          (hostname
           (make-instance 'api-context))))
  (is
   (equal "https://example.com"
          (hostname
           (make-instance 'api-context
                          :hostname "https://example.com")))))

(test api-feature-enabled-p
  (is-true
   (api-feature-enabled-p
    (make-instance 'test-api-context
                   :features (list "foobar"))
    :foobar))
  (is-false
   (api-feature-enabled-p
    (make-instance 'test-api-context
                   :features (list "carbar"))
    :foobar)))

(test fixes-api-hostname
  (is
   (equal "https://foo.screenshotbot.io"
          (hostname
           (make-instance 'api-context
                          :hostname "foo.screenshotbot.io")))))

(test extract-hostname-from-secret
  ;; Test extracting hostname from a valid encoded secret
  (let* ((secret "1234567890123456789012345678901234567890") ;; 40 chars
         (full-secret (make-encoded-secret "TESTKEY123" "https://custom.example.com" secret)))
    (is (= 40 (length secret)))
    (is (equal "https://custom.example.com"
               (extract-hostname-from-secret full-secret))))

  ;; Test that plain secrets (without encoding) return nil
  (is (equal nil
             (extract-hostname-from-secret "plainSecretKey1234567890123456789012")))

  ;; Test nil secret
  (is (equal nil
             (extract-hostname-from-secret nil)))

  ;; Test empty secret
  (is (equal nil
             (extract-hostname-from-secret ""))))

(test api-context-uses-hostname-from-secret
  ;; Test that api-context extracts hostname from the secret when no hostname is provided
  (let* ((secret "1234567890123456789012345678901234567890")
         (full-secret (make-encoded-secret "TESTKEY123" "https://abcd.example.com" secret)))
    (is (= 40 (length secret)))
    (is (equal "https://abcd.example.com"
               (hostname
                (make-instance 'api-context
                               :key "TESTKEY123"
                               :secret full-secret
                               :hostname ""))))))

(test api-context-prefers-explicit-hostname
    ;; Test that explicit hostname takes precedence
  (let* ((secret "1234567890123456789012345678901234567890")
         (full-secret (make-encoded-secret "TESTKEY123" "https://abcd.example.com" secret)))
    (is (= 40 (length secret)))
    (is (equal "https://override.example.com"
               (hostname
                (make-instance 'api-context
                               :key "TESTKEY123"
                               :secret full-secret
                               :hostname "https://override.example.com"))))))

(test api-context-extracts-key-from-secret
  ;; Test that api-context extracts api-key from the secret when no key is provided
  (let* ((actual-secret "1234567890123456789012345678901234567890")
         (full-secret (make-encoded-secret "TESTKEY123" "https://custom.example.com" actual-secret)))
    (is (= 40 (length actual-secret)))
    (let ((ctx (make-instance 'api-context
                              :secret full-secret
                              :hostname "")))
      (is (equal "TESTKEY123" (key ctx)))
      ;; The secret should remain the full encoded secret, not just the 40-char part
      (is (equal full-secret (secret ctx)))
      (is (equal "https://custom.example.com" (hostname ctx))))))

(test api-context-prefers-explicit-key
  ;; Test that explicit key takes precedence
  (let* ((actual-secret "1234567890123456789012345678901234567890")
         (full-secret (make-encoded-secret "TESTKEY123" "https://custom.example.com" actual-secret)))
    (is (= 40 (length actual-secret)))
    (let ((ctx (make-instance 'api-context
                              :key "OVERRIDE_KEY"
                              :secret full-secret
                              :hostname "")))
      (is (equal "OVERRIDE_KEY" (key ctx)))
      ;; When key is provided, we don't extract, so secret stays as-is
      (is (equal full-secret (secret ctx))))))

