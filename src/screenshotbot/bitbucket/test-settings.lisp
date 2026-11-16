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
                #:access-token-for-args
                #:bitbucket-token
                #:delete-superseded-tokens
                #:cached-access-token
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

(def-fixture state (&key globally)
  (let ((auto-restart:*global-enable-auto-retries-p* nil))
   (with-installation ()
    (with-test-store (:globally globally)
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
    (let ((token (car (bitbucket-settings-for-company company))))
      (update-from-refresh-token company '((:refresh--token . "bar")))
      (is (equal (list token) (bitbucket-settings-for-company company))))
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
      (is (equal 1 (length (bitbucket-settings-for-company company))))
      (is (eql user
               (bitbucket-token-actor (car (bitbucket-settings-for-company company))))))))

;; Mocks the access-token exchange: each call returns a new access token
;; and a rotated refresh token, and counts calls in CALLS.
(defmacro with-fake-exchange ((calls &key (expires-in 7200) (delay 0)) &body body)
  `(let ((,calls 0)
         (lock (bt:make-lock)))
     (cl-mock:with-mocks ()
       (cl-mock:if-called 'access-token-for-args
                          (lambda (args &key company)
                            (declare (ignore args company))
                            (sleep ,delay)
                            (let ((n (bt:with-lock-held (lock)
                                       (incf ,calls))))
                              `((:access--token . ,(format nil "access-~a" n))
                                (:refresh--token . ,(format nil "refresh-~a" n))
                                (:expires--in . ,,expires-in)))))
       ,@body)))

(test cached-access-token-is-reused
  (with-fixture state ()
    (update-from-refresh-token company '((:refresh--token . "refresh-0")))
    (let ((token (car (bitbucket-settings-for-company company))))
      (with-fake-exchange (calls)
        (is (equal "access-1" (cached-access-token token)))
        (is (equal "access-1" (cached-access-token token)))
        (is (equal 1 calls))
        (is (equal "refresh-1" (refresh-token token)))))))

(test cached-access-token-is-refreshed-when-about-to-expire
  (with-fixture state ()
    (update-from-refresh-token company '((:refresh--token . "refresh-0")))
    (let ((token (car (bitbucket-settings-for-company company))))
      (with-fake-exchange (calls :expires-in 60)
        (is (equal "access-1" (cached-access-token token)))
        (is (equal "access-2" (cached-access-token token)))
        (is (equal 2 calls))))))

(test concurrent-callers-share-one-refresh
  (with-fixture state (:globally t)
    (update-from-refresh-token company '((:refresh--token . "refresh-0")))
    (let ((token (car (bitbucket-settings-for-company company))))
      (with-fake-exchange (calls :delay 0.2)
        (let* ((results (make-array 5 :initial-element nil))
               (threads
                 (loop for i below 5
                       collect (let ((i i))
                                 (bt:make-thread
                                  (lambda ()
                                    (setf (aref results i)
                                          (cached-access-token token))))))))
          (mapc #'bt:join-thread threads)
          (is (equal 1 calls))
          (is (every (lambda (x) (equal "access-1" x)) results)))))))

(test delete-superseded-tokens
  (with-fixture state ()
    (let ((other-company (make-instance 'company)))
      (flet ((make-token (company name)
               (make-instance 'bitbucket-token
                              :refresh-token name
                              :company company)))
        (make-token company "old-1")
        (make-token company "old-2")
        (let ((newest (make-token company "newest"))
              (other (make-token other-company "other")))
          (delete-superseded-tokens)
          (is (equal (list newest) (bitbucket-settings-for-company company)))
          (is (equal (list other) (bitbucket-settings-for-company other-company))))))))

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
