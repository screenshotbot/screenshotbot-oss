;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-threading
  (:use #:cl
        #:fiveam
        #:util/threading)
  (:import-from #:util/threading
                #:funcall-with-sentry-logs)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-threading)


(util/fiveam:def-suite)

(test simple-create-thread
  (let ((var nil))
    (let ((thread (util:make-thread
                   (lambda ()
                     (setf var t)))))
      (bt:join-thread thread)
      (is-true var))))


(test safe-interrupt
  (let* ((ctr 0)
         (max-ctr 10)
         (callback-called-p nil)
         (thread (bt:make-thread
                  (lambda ()
                    (with-safe-interruptable (:on-quit (lambda ()
                                                         (setf callback-called-p t)))
                      (loop for i below max-ctr
                            do
                               (incf ctr)
                               (safe-interrupt-checkpoint)
                               (sleep 0.1)))))))
    (safe-interrupt thread)
    (bt:join-thread thread)
    (is-true callback-called-p)
    (is (< ctr (/ max-ctr 2)))
    (pass)))

(def-fixture sentry-mocks ()
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

(test sentry-logging-for-make-thread
  (with-fixture sentry-mocks ()
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
  (with-fixture sentry-mocks ()
    (funcall-with-sentry-logs
     (lambda ()
       (loop for i below 100 do
         (warn
          'my-simple-warning))))
    (is (eql 5 (length sentry-logs)))))
