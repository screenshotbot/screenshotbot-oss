;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-server
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/server
                #:funcall-with-sentry-logs
                #:log-sentry)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-server)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (let ((hunchentoot:*catch-errors-p* t))
      (let ((sentry-logs nil))
        #-screenshotbot-oss
        (cl-mock:if-called
         'sentry-client:capture-exception
          (lambda (e)
            (push e sentry-logs)))
       (&body)))))

(define-condition my-simple-warning (warning)
  ())

(define-condition my-simple-error (error)
  ())

#-screenshotbot-oss
(test sentry-logging-for-make-thread
  (with-fixture state ()
    (funcall-with-sentry-logs
     (lambda ()
       (warn 'my-simple-warning)))
    (is (eql 1 (length sentry-logs)))
    (signals my-simple-error
     (funcall-with-sentry-logs
      (lambda ()
        (error 'my-simple-error))))
    (is (eql 2 (length sentry-logs)))))

#-screenshotbot-oss
(test large-number-of-warnings
  (with-fixture state ()
    (funcall-with-sentry-logs
     (lambda ()
       (loop for i below 100 do
         (warn
          'my-simple-warning))))
    (is (eql 5 (length sentry-logs)))))
