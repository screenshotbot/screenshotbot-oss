;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-request
  (:use #:cl
        #:fiveam)
  (:import-from #:cl-mock
                #:if-called)
  (:import-from #:fiveam-matchers/errors
                #:error-with-string-matching
                #:signals-error-matching)
  (:import-from #:screenshotbot/sdk/request
                #:api-error
                #:ensure-api-success
                #:%request)
  (:import-from #:screenshotbot/api/model
                #:*api-version*)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:screenshotbot/api/recorder-run
                #:*synchronous-promotion*)
  (:import-from #:screenshotbot/api/core
                #:*wrap-internal-errors*)
  (:import-from #:util/request
                #:http-request)
  (:local-nicknames (#:a #:alexandria)
                    (#:api-key #:core/api/model/api-key)
                    (#:dto #:screenshotbot/api/model)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:run-context #:screenshotbot/sdk/run-context)))
(in-package :screenshotbot/sdk/test-request)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*synchronous-promotion* t)
        (*wrap-internal-errors* nil))
    (cl-mock:with-mocks ()
      (if-called 'warmup-image-caches
                 (lambda (run)
                   (declare (ignore run))))
      (let ((auto-restart:*global-enable-auto-retries-p* nil))
        (util:copying (flags:*pull-request*
                       flags:*override-commit-hash*
                       flags:*main-branch*)
          (&body))))))


(test retries-%request
  (with-fixture state ()
    (let ((auto-restart:*global-enable-auto-retries-p* t))
     (let ((count 0))
       (cl-mock:if-called 'http-request
                          (lambda (url &rest args)
                            (incf count)
                            (cond
                              ((<= count 3)
                               (values (make-string-input-stream "Bad") 502))
                              (t
                               (values (make-string-input-stream "Good") 200)))))
       (is
        (equal "Good"
               (%request (make-instance 'api-context
                                        :remote-version *api-version*
                                        :key "foo"
                                        :secret "bar"
                                        :hostname "https://example.com")
                         "/api/test"
                         :backoff 0)))))))

(test retries-%request-for-error
  (with-fixture state ()
    (let ((auto-restart:*global-enable-auto-retries-p* t))
     (let ((count 0))
       (cl-mock:if-called 'http-request
                          (lambda (url &rest args)
                            (incf count)
                            (cond
                              ((<= count 3)
                               (error "some error happened"))
                              (t
                               (values (make-string-input-stream "Good") 200)))))
       (is
        (equal "Good"
               (%request (make-instance 'api-context
                                        :remote-version *api-version*
                                        :key "foo"
                                        :secret "bar"
                                        :hostname "https://example.com")
                         "/api/test"
                         :backoff 0)))))))

(test retries-%request-will-finally-fail
  (with-fixture state ()
    (let ((auto-restart:*global-enable-auto-retries-p* t))
     (let ((count 0))
       (cl-mock:if-called 'http-request
                          (lambda (url &rest args)
                            (incf count)
                            (cond
                              ((<= count 10)
                               (error "some error happened"))
                              (t
                               (values (make-string-input-stream "Good") 200)))))
       (signals-error-matching ()
            (%request (make-instance 'api-context
                                     :remote-version *api-version*
                                     :key "foo"
                                     :secret "bar"
                                     :hostname "https://example.com")
                      "/api/test"
                      :backoff 0)
            (error-with-string-matching "some error happened"))))))

(test ensure-api-success
  (finishes
    (ensure-api-success `((:result . "foobar"))))
  (signals api-error
   (ensure-api-success `((:error . "foobar")))))
