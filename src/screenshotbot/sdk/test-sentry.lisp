;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-sentry
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/sentry
                #:post-crash
                #:with-sentry)
  (:import-from #:screenshotbot/sdk/api-context
                #:*crash-api-context*
                #:api-context)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:cl-mock
                #:answer
                #:if-called
                #:with-mocks)
  (:import-from #:util/threading
                #:*tags*
                #:*extras*))
(in-package :screenshotbot/sdk/test-sentry)

(util/fiveam:def-suite)

(define-condition my-error (error)
  ())

(test happy-path
  (finishes
    (with-sentry (:dry-run t
                      :on-error (lambda ()))
      (+ 1 1)))
  (signals my-error
    (with-sentry (:dry-run t
                  :stream (make-string-output-stream)
                  :on-error (lambda ()))
      (error 'my-error))))

(test all-tags-and-extras-evaluate
  (finishes
    (with-sentry (:dry-run t
                  :on-error (lambda ()))
      (loop for fn in *extras*
            do (funcall fn (make-condition 'my-error)))
      (loop for fn in *tags*
            do (funcall fn (make-condition 'my-error))))))

(def-fixture crash-state (&key (http-request-fn nil) (ignore-sentry nil))
  (with-mocks ()
    (let ((requests nil)
          (direct-events nil))
      (answer (uiop:getenv "TDRHQ_IGNORE_SENTRY") ignore-sentry)
      (if-called 'http-request
                 (or http-request-fn
                     (lambda (url &rest args)
                       (push (list* url args) requests)
                       "{}")))
      (if-called 'sentry-client::post-sentry-request
                 (lambda (data &optional client)
                   (declare (ignore client))
                   (push data direct-events)))
      (&body))))

(defun make-context (&optional remote-version)
  (make-instance 'api-context
                 :key "foo"
                 :secret "bar"
                 :hostname "https://example.com"
                 :remote-version remote-version))

(test post-crash-without-an-api-context
  (with-fixture crash-state ()
    (let ((*crash-api-context* nil))
      (post-crash "{}"))
    (destructuring-bind ((url &key content basic-authorization &allow-other-keys))
        requests
      (is (equal "https://api.screenshotbot.io/api/crash" url))
      (is (equal "{}" content))
      (is (eql nil basic-authorization)))))

(test post-crash-with-an-api-context
  (with-fixture crash-state ()
    (let ((*crash-api-context* (make-context)))
      (post-crash "{}"))
    (destructuring-bind ((url &key basic-authorization &allow-other-keys))
        requests
      (is (equal "https://example.com/api/crash" url))
      (is (equal '("foo" "bar") basic-authorization)))))

(test post-crash-respects-ignore-sentry
  (with-fixture crash-state (:ignore-sentry "true")
    (post-crash "{}")
    (is (eql nil requests))))

#-screenshotbot-oss
(progn
  (defun capture-with-context (context)
    (let ((*crash-api-context* context))
      (sentry-client::client-capture-exception
       (make-instance 'screenshotbot/sdk/sentry::api-crash-client
                      :dsn "https://foo:bar@sentry.io/1")
       (make-condition 'my-error))))

  (test without-an-api-context-goes-via-the-api
    (with-fixture crash-state ()
      (capture-with-context nil)
      (destructuring-bind ((url &key content &allow-other-keys))
          requests
        (is (equal "https://api.screenshotbot.io/api/crash" url))
        (is (str:containsp "MY-ERROR" content)))
      (is (eql nil direct-events))))

  (test server-with-support-goes-via-the-api
    (with-fixture crash-state ()
      (capture-with-context (make-context 22))
      (destructuring-bind ((url &key &allow-other-keys))
          requests
        (is (equal "https://example.com/api/crash" url)))
      (is (eql nil direct-events))))

  (test older-server-goes-directly-to-sentry
    (with-fixture crash-state ()
      (capture-with-context (make-context 21))
      (is (eql nil requests))
      (is (eql 1 (length direct-events)))
      (is (str:containsp "MY-ERROR" (car direct-events)))))

  (test unknown-version-goes-via-the-api-without-fetching-it
    (with-fixture crash-state ()
      (capture-with-context (make-context nil))
      (destructuring-bind ((url &key &allow-other-keys))
          requests
        (is (equal "https://example.com/api/crash" url)))
      (is (eql nil direct-events))))

  (test failures-going-via-the-api-do-not-propagate
    (with-fixture crash-state (:http-request-fn (lambda (url &rest args)
                                                  (declare (ignore url args))
                                                  (error "connection refused")))
      (finishes
        (capture-with-context (make-context 22)))))

  (test failures-going-directly-do-not-propagate
    (with-mocks ()
      (if-called 'sentry-client::post-sentry-request
                 (lambda (data &optional client)
                   (declare (ignore data client))
                   (error "connection refused")))
      (finishes
        (capture-with-context (make-context 21))))))
