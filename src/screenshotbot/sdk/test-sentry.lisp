;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-sentry
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/sentry
                #:*crash-pool*
                #:flush-crash-reports
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
                #:max-pool
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

(def-fixture crash-state (&key (http-request-fn nil)
                               (before-request (lambda ()))
                               (blockedp nil)
                               (ignore-sentry nil))
  (with-mocks ()
    (let ((requests nil)
          (direct-events nil)
          (release (lparallel:promise))
          (started (lparallel:promise))
          (*crash-pool* (make-instance 'max-pool :max 5)))
      (declare (ignorable release started))
      (answer (uiop:getenv "TDRHQ_IGNORE_SENTRY") ignore-sentry)
      (if-called 'http-request
                 (or http-request-fn
                     (lambda (url &rest args)
                       (when blockedp
                         ;; Requests wait here until we release them.
                         (lparallel:fulfill started t)
                         (lparallel:force release))
                       (funcall before-request)
                       (push (list* url args) requests)
                       "{}")))
      (if-called 'sentry-client::post-sentry-request
                 (lambda (data &optional client)
                   (declare (ignore client))
                   (push data direct-events)))
      (unwind-protect
           (&body)
        (lparallel:fulfill release t)
        (flush-crash-reports)))))

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

  (defun capture-and-flush (context)
    (capture-with-context context)
    (flush-crash-reports))

  (test without-an-api-context-goes-via-the-api
    (with-fixture crash-state ()
      (capture-and-flush nil)
      (destructuring-bind ((url &key content &allow-other-keys))
          requests
        (is (equal "https://api.screenshotbot.io/api/crash" url))
        (is (str:containsp "MY-ERROR" content)))
      (is (eql nil direct-events))))

  (test server-with-support-goes-via-the-api
    (with-fixture crash-state ()
      (capture-and-flush (make-context 22))
      (destructuring-bind ((url &key &allow-other-keys))
          requests
        (is (equal "https://example.com/api/crash" url)))
      (is (eql nil direct-events))))

  (test older-server-goes-directly-to-sentry
    (with-fixture crash-state ()
      (capture-and-flush (make-context 21))
      (is (eql nil requests))
      (is (eql 1 (length direct-events)))
      (is (str:containsp "MY-ERROR" (car direct-events)))))

  (test unknown-version-goes-via-the-api-without-fetching-it
    (with-fixture crash-state ()
      (capture-and-flush (make-context nil))
      (destructuring-bind ((url &key &allow-other-keys))
          requests
        (is (equal "https://example.com/api/crash" url)))
      (is (eql nil direct-events))))

  (test failures-going-via-the-api-do-not-propagate
    (with-fixture crash-state (:http-request-fn (lambda (url &rest args)
                                                  (declare (ignore url args))
                                                  (error "connection refused")))
      (finishes
        (capture-and-flush (make-context 22)))))

  (test failures-going-directly-do-not-propagate
    (with-fixture crash-state ()
      (with-mocks ()
        (if-called 'sentry-client::post-sentry-request
                   (lambda (data &optional client)
                     (declare (ignore data client))
                     (error "connection refused")))
        (finishes
          (capture-and-flush (make-context 21))))))

  (test does-not-block-the-thread-that-crashed
    (with-fixture crash-state (:blockedp t)
      (capture-with-context (make-context 22))
      (lparallel:force started)
      (is (eql nil requests))))

  (test flush-waits-for-reports-in-progress
    (with-fixture crash-state (:blockedp t)
      (capture-with-context (make-context 22))
      (lparallel:force started)
      (let ((flushedp nil)
            (pool *crash-pool*))
        (let ((thread (bt:make-thread
                       (lambda ()
                         (let ((*crash-pool* pool))
                           (flush-crash-reports))
                         (setf flushedp t)))))
          (sleep 0.3)
          (is (eql nil flushedp))
          (is (eql nil requests))
          (lparallel:fulfill release t)
          (bt:join-thread thread)
          (is (eql t flushedp))
          (is (eql 1 (length requests)))))))

  (test flush-waits-for-reports-still-in-the-queue
    (with-fixture crash-state (:before-request (lambda () (sleep 0.05)))
      (let ((*crash-pool* (make-instance 'max-pool :max 1)))
        (dotimes (i 8)
          (capture-with-context (make-context 22)))
        (flush-crash-reports)
        (is (eql 8 (length requests))))))

  (test flush-gives-up-on-a-report-that-hangs
    (with-fixture crash-state (:blockedp t)
      (capture-with-context (make-context 22))
      (lparallel:force started)
      (finishes
        (flush-crash-reports :timeout 1))
      (is (eql nil requests))))

  (test the-report-has-the-stacktrace-of-the-crashing-thread
    (with-fixture crash-state ()
      (capture-and-flush (make-context 22))
      (destructuring-bind ((url &key content &allow-other-keys))
          requests
        (declare (ignore url))
        (is (str:containsp "CAPTURE-WITH-CONTEXT" content)))))

  (test warning-while-reporting-does-not-get-reported
    (with-fixture crash-state (:before-request
                               (lambda ()
                                 (warn "Server is making us close the stream")))
      (capture-and-flush (make-context 22))
      (is (eql 1 (length requests)))))

  (test keeps-the-api-context-of-the-crashing-thread
    (with-fixture crash-state ()
      (let ((*crash-api-context* (make-context 22)))
        (capture-with-context *crash-api-context*))
      (flush-crash-reports)
      (destructuring-bind ((url &key &allow-other-keys))
          requests
        (is (equal "https://example.com/api/crash" url))))))
