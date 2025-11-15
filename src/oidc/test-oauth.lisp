;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :oidc/test-oauth
  (:use :cl
        :fiveam)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:is-equal-to)
  (:import-from #:oidc/oauth
                #:make-oauth-url)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:cl-mock
                #:if-called)
  (:local-nicknames (#:a #:alexandria)))
(in-package :oidc/test-oauth)

(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (with-fake-request ()
     (auth:with-sessions ()
       (&body)))))

(test make-oauth-url-basic
  (with-fixture state ()
    (with-fake-request (:host "example.com")
      (let* ((oauth-url))
        (cl-mock:if-called 'nibble:nibble-id
                           (lambda (nibble)
                             (declare (ignore nibble))
                             12345))
        (setf oauth-url (make-oauth-url
                         "https://oauth.provider.com/authorize?client_id=abc123&scope=read"
                         (lambda (&key code error redirect-uri)
                           (declare (ignore code error redirect-uri)))))
        (let ((parsed (quri:uri oauth-url)))
          (is (equal (quri:uri-scheme parsed) "https"))
          (is (equal (quri:uri-host parsed) "oauth.provider.com"))
          (is (equal (quri:uri-path parsed) "/authorize"))
          (let ((params (quri:uri-query-params parsed)))
            (assert-that (a:assoc-value params "client_id" :test #'equal)
                         (is-equal-to "abc123"))
            (assert-that (a:assoc-value params "scope" :test #'equal)
                         (is-equal-to "read"))
            (assert-that (a:assoc-value params "redirect_uri" :test #'equal)
                         (is-equal-to "http://example.com/account/oauth-callback"))
            (assert-that (a:assoc-value params "state" :test #'equal)
                         (is-equal-to "12345"))))))))

(test make-oauth-url-without-existing-params
  (with-fixture state ()
    (with-fake-request (:host "example.com")
      (let* ((oauth-url))
        (cl-mock:if-called 'nibble:nibble-id
                           (lambda (nibble)
                             (declare (ignore nibble))
                             67890))
        (setf oauth-url (make-oauth-url
                         "https://oauth.provider.com/authorize"
                         (lambda (&key code error redirect-uri)
                           (declare (ignore code error redirect-uri)))))
        (let ((parsed (quri:uri oauth-url)))
          (is (equal (quri:uri-path parsed) "/authorize"))
          (let ((params (quri:uri-query-params parsed)))
            (is (= (length params) 2))
            (assert-that (a:assoc-value params "redirect_uri" :test #'equal)
                         (is-equal-to "http://example.com/account/oauth-callback"))
            (assert-that (a:assoc-value params "state" :test #'equal)
                         (is-equal-to "67890"))))))))

(test make-oauth-with-via
  (with-fixture state ()
    (with-fake-request (:host "example.com")
      (let* ((oauth-url))
        (cl-mock:if-called 'nibble:nibble-id
                           (lambda (nibble)
                             (declare (ignore nibble))
                             12345))
        (setf oauth-url (make-oauth-url
                         "/authorize?client_id=abc123&scope=read"
                         (lambda (&key code error redirect-uri)
                           (declare (ignore code error redirect-uri)))
                         :via "https://screenshotbot.io"))
        (let ((parsed (quri:uri oauth-url)))
          (let ((params (quri:uri-query-params parsed)))
            (assert-that (a:assoc-value params "client_id" :test #'equal)
                         (is-equal-to "abc123"))
            (assert-that (a:assoc-value params "scope" :test #'equal)
                         (is-equal-to "read"))
            (assert-that (a:assoc-value params "redirect_uri" :test #'equal)
                         (is-equal-to "https://screenshotbot.io/account/oauth-callback"))
            (assert-that (a:assoc-value params "state" :test #'equal)
                         (is-equal-to "12345,http://example.com/"))))))))
