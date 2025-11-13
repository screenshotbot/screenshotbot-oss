;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-common
  (:use :cl)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:has-typep
                #:is-equal-to)
  (:import-from #:it.bese.fiveam
                #:is
                #:signals
                #:def-fixture
                #:test
                #:with-fixture)
  (:import-from #:screenshotbot/login/common
                #:illegal-oauth-redirect
                #:server-with-login
                #:signin-get)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/login/test-common)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (cl-mock:with-mocks ()
       (&body)))))

(test server-with-login-happy-path
  (with-fixture state ()
    (let ((last-redirect))
      (cl-mock:if-called 'signin-get
                         (lambda (&key alert redirect)
                           (setf last-redirect redirect)))
      (server-with-login
       (lambda () "foo")
       :needs-login t)
      (assert-that last-redirect
                   (has-typep 'nibble:nibble)))))

(test server-with-login-with-allowing-url-redirect
  (with-fixture state ()
    (let ((last-redirect))
      (cl-mock:if-called 'signin-get
                         (lambda (&key alert redirect)
                           (setf last-redirect redirect)))
      (with-fake-request (:script-name "/foo/bar")
        (server-with-login
         (lambda () "foo")
         :allow-url-redirect t
         :needs-login t))
      (assert-that last-redirect
                   (is-equal-to "/foo/bar")))))

(test handle-oauth-callback-happy-path
  (with-fixture state ()
    (let ((callback-result)
          (rendered-nibble))
      (cl-mock:if-called 'nibble:render-nibble
                         (lambda (self state)
                           (setf rendered-nibble state)
                           "rendered-content"))
      (with-fake-request ()
        (setf callback-result
              (screenshotbot/login/common::handle-oauth-callback
               (make-instance 'auth:auth-acceptor-mixin)
               "test-code"
               "12345")))
      (assert-that callback-result
                   (is-equal-to "rendered-content"))
      (assert-that rendered-nibble
                   (is-equal-to "12345")))))


(test handle-oauth-with-unpermitted-redirect
  (with-fixture state ()
    (let ((callback-result)
          (rendered-nibble))
      (cl-mock:if-called 'nibble:render-nibble
                         (lambda (self state)
                           (setf rendered-nibble state)
                           "rendered-content"))
      (with-fake-request ()
        (signals illegal-oauth-redirect
          (screenshotbot/login/common::handle-oauth-callback
           (make-instance 'auth:auth-acceptor-mixin)
           "test-code"
           "12345,https://attacker.example.com"))))))

(test handle-oauth-with-permitted-redirect
  (with-fixture state ()
    (let ((callback-result)
          (rendered-nibble))
      (cl-mock:if-called 'screenshotbot/login/common::allow-oauth-redirect-p
                         (lambda (installation redirect)
                           (declare (ignore installation))
                           (equal redirect "https://trusted.example.com")))
      (with-fake-request ()
        (catch 'hunchentoot::handler-done
          (screenshotbot/login/common::handle-oauth-callback
           (make-instance 'auth:auth-acceptor-mixin)
           "test-code"
           "12345,https://trusted.example.com")
          (fail "expected to redirect"))
        (is (equal (hunchentoot:header-out :location hunchentoot:*reply*)
                   "https://trusted.example.com/account/oauth-callback?state=12345&code=test-code"))))))



