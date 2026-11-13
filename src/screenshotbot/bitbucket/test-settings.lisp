;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/bitbucket/test-settings
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/pro/bitbucket/settings
                #:%bitbucket-oauth-callback
                #:same-origin-p
                #:bitbucket-token-actor
                #:update-from-refresh-token
                #:refresh-token
                #:bitbucket-settings-for-company)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/bitbucket/test-settings)

(util/fiveam:def-suite)

(def-fixture state ()
  (let ((auto-restart:*global-enable-auto-retries-p* nil))
   (with-installation ()
    (with-test-store ()
      (let ((company (make-instance 'company)))
        (&body))))))

(test preconditions
  (with-fixture state ()
    (update-from-refresh-token company
                               '((:refresh--token . "foo")))
    (is (equal 1 (length (bitbucket-settings-for-company company))))
    (is (equal
         "foo"
         (refresh-token (car (bitbucket-settings-for-company company)))))))

(test updating-refresh-token
  (with-fixture state ()
    (update-from-refresh-token company '((:refresh--token . "foo")))
    (update-from-refresh-token company '((:refresh--token . "bar")))
    (is (equal 2 (length (bitbucket-settings-for-company company))))
    (is (equal
         "bar"
         (refresh-token (car (bitbucket-settings-for-company company)))))))

(test when-refresh-token-doesnt-change
  (with-fixture state ()
    (update-from-refresh-token company '((:refresh--token . "foo")))
    (update-from-refresh-token company '((:refresh--token . "foo")))
    (is (equal 1 (length (bitbucket-settings-for-company company))))
    (is (equal
         "foo"
         (refresh-token (car (bitbucket-settings-for-company company)))))))

(test records-actor
  (with-fixture state ()
    (let ((user (make-user)))
      (update-from-refresh-token company '((:refresh--token . "foo"))
                                 :actor user)
      (is (eql user
               (bitbucket-token-actor (car (bitbucket-settings-for-company company))))))))

(test actor-is-nil-when-slot-is-unbound
  (with-fixture state ()
    (update-from-refresh-token company '((:refresh--token . "foo")))
    (let ((token (car (bitbucket-settings-for-company company))))
      (slot-makunbound token 'screenshotbot/bitbucket/settings::actor)
      (is (null (bitbucket-token-actor token))))))

(test refreshed-token-keeps-actor
  (with-fixture state ()
    (let ((user (make-user)))
      (update-from-refresh-token company '((:refresh--token . "foo"))
                                 :actor user)
      (update-from-refresh-token company '((:refresh--token . "bar")))
      (is (equal 2 (length (bitbucket-settings-for-company company))))
      (is (eql user
               (bitbucket-token-actor (car (bitbucket-settings-for-company company))))))))

(test same-origin-p
  (is-true (same-origin-p "https://staging.screenshotbot.io"
                          "https://staging.screenshotbot.io/"))
  (is-false (same-origin-p "https://staging.screenshotbot.io"
                           "https://screenshotbot.io"))
  (is-false (same-origin-p "https://localhost:8080"
                           "https://localhost:9090"))
  (is-false (same-origin-p "http://staging.screenshotbot.io"
                           "https://staging.screenshotbot.io")))

(test oauth-callback-on-same-domain-renders-nibble
  (let ((*installation* (make-instance 'installation
                                       :domain "https://staging.screenshotbot.io"))
        (hunchentoot:*acceptor* nil))
    (cl-mock:with-mocks ()
      (cl-mock:if-called 'encrypt:decrypt-number
                         (lambda (x)
                           (is (equal "encrypted-id" x))
                           23))
      (cl-mock:if-called 'nibble:render-nibble
                         (lambda (acceptor id)
                           (declare (ignore acceptor))
                           (format nil "rendered ~a" id)))
      (is (equal "rendered 23"
                 (%bitbucket-oauth-callback
                  "code"
                  "encrypted-id,https://staging.screenshotbot.io"))))))

(test oauth-callback-on-unknown-domain-errors
  (let ((*installation* (make-instance 'installation
                                       :domain "https://staging.screenshotbot.io")))
    (signals error
      (%bitbucket-oauth-callback
       "code"
       "encrypted-id,https://evil.example.com"))))
