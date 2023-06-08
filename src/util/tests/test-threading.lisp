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
                #:*trace-stream*
                #:max-pool
                #:build-extras
                #:with-extras
                #:*log-sentry-p*
                #:ignore-error
                #:%invoke-debugger
                #:handle-error
                #:*catch-errors-p*
                #:funcall-with-sentry-logs)
  (:import-from #:util/testing
                #:with-global-binding)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-threading)


(util/fiveam:def-suite)

(defvar *ctr* 0)

(def-fixture state ()
  (let ((stream (make-string-output-stream)))
   (with-global-binding ((*catch-errors-p* t)
                         (*debugger-hook* *debugger-hook*)
                         (*trace-stream* stream))
     (unwind-protect
          (&body)
       (setf *ctr* 0)))))

(test simple-create-thread
  (with-fixture state ()
   (let ((var nil))
     (let ((thread (util:make-thread
                    (lambda ()
                      (setf var t)))))
       (bt:join-thread thread)
       (is-true var)))))


#+nil ;; Very flaky test
(test safe-interrupt
  (with-fixture state ()
   (let* ((ctr 0)
          (max-ctr 300)
          (lock (bt:make-lock))
          (cv (bt:make-condition-variable))
          (callback-called-p nil))

     (bt:with-lock-held (lock)
      (let ((thread (bt:make-thread
                     (lambda ()
                       (with-safe-interruptable (:on-quit (lambda ()
                                                            (setf callback-called-p t)))
                         (bt:with-lock-held (lock)
                           (bt:condition-notify cv))
                         (loop for i below max-ctr
                               do
                                  (incf ctr)
                                  (safe-interrupt-checkpoint)
                                  (sleep 0.1)))))))
        (bt:condition-wait cv lock)
        (safe-interrupt thread)
        (bt:join-thread thread)
        (is-true callback-called-p)
        (is (< ctr (/ max-ctr 2))))))))

(def-fixture sentry-mocks ()
  (cl-mock:with-mocks ()
    ;; don't print the stack traces to the test output
    (cl-mock:if-called 'trivial-backtrace:print-backtrace
                        (lambda (e &key output)))

    (let ((hunchentoot:*catch-errors-p* t))
      (let ((sentry-logs nil))
        #-screenshotbot-oss
        (cl-mock:if-called
         'sentry-client:capture-exception
         (lambda (e &key extras)
           (declare (ignore extras))
           (push e sentry-logs)))
       (&body)))))

(define-condition my-simple-warning (warning)
  ())

(define-condition my-simple-error (error)
  ())

#-screenshotbot-oss
(test sentry-logging-for-make-thread
  (with-fixture state ()
   (with-fixture sentry-mocks ()
     (funcall-with-sentry-logs
      (lambda ()
        (warn 'my-simple-warning)))
     (is (eql 1 (length sentry-logs)))
     (signals my-simple-error
       (funcall-with-sentry-logs
        (lambda ()
          (error 'my-simple-error))))
     (is (eql 2 (length sentry-logs))))))

#-screenshotbot-oss
(test large-number-of-warnings
  (with-fixture state ()
   (with-fixture sentry-mocks ()
     (funcall-with-sentry-logs
      (lambda ()
        (loop for i below 100 do
          (warn
           'my-simple-warning))))
     (is (eql 5 (length sentry-logs))))))


(test handle-error-if-not-catch-errors-p
  (with-fixture state ()
    (with-fixture sentry-mocks ()
      (let (debugger-hook-called-p)
        (setf *catch-errors-p* nil)
        (setf *debugger-hook* (lambda (e old-debugger-hook)
                                (error "never called")))
        (cl-mock:if-called '%invoke-debugger
                            (lambda (e)
                              (setf debugger-hook-called-p :seen-1)))
        (restart-case
            (handle-error (make-instance 'error))
          (ignore-error ()
            (fail "got abort, when we shouldn't have")))
        (is-true debugger-hook-called-p)))))

(test handle-error-if-catch-errors-p
  (with-fixture state ()
    (with-fixture sentry-mocks ()
      (let (debugger-hook-called-p
            abort-called-p)
        (setf *catch-errors-p* t)
        (setf *debugger-hook* (lambda (e h)))
        (cl-mock:if-called '%invoke-debugger
                            (lambda (e)
                              (setf debugger-hook-called-p :seen)))
        (restart-case
            (handle-error (make-instance 'error))
          (ignore-error ()
            (setf abort-called-p t)))
        (is-false debugger-hook-called-p)
        (is-true abort-called-p)))))


(test ignore-and-log-errors-when-not-used-at-toplevel
  (with-fixture state ()
    (with-global-binding ((*log-sentry-p* nil))
     (let ((called nil))
       (let ((thread (bt:make-thread
                      (lambda ()
                        (ignore-and-log-errors ()
                          (error "blah blah from test-threading"))
                        (setf called t)))))
         (bt:join-thread thread)
         (is-true called))))))


(test with-extras
  (with-fixture state ()
   (let ((body-called-p nil))
     (with-extras (("name" "bleh"))
       (setf body-called-p t)
       (assert-that
        (build-extras nil)
        (contains '("name" . "bleh"))))
     (is-true body-called-p))))

(test extras-are-bound-late
  (with-fixture state ()
    (let ((count 0))
      (with-extras (("name" count))
        (incf count)
        (assert-that (build-extras nil)
                     (contains '("name" . "1")))
        (incf count)
        (assert-that (build-extras nil)
                     (contains '("name" . "2")))))))

(defvar *lock* (bt:make-lock))

(test max-pool
  (with-fixture state ()
    (let ((max-pool (make-instance 'max-pool :max 3))
          (thread-count 100))
      (let* ((promises (loop for i from 0 below thread-count
                             collect (make-thread
                                      (lambda ()
                                        (bt:with-lock-held (*lock*)
                                          (incf *ctr*)))
                                      :pool max-pool)))
             (threads (loop for promise in promises
                            collect (lparallel:force promise))))
        (dolist (thread threads)
          (bt:join-thread thread))
        (is (eql thread-count *ctr*))))))


(test max-pool-with-errors
  (with-fixture state ()
    (with-global-binding ((*log-sentry-p* nil))
     (let ((max-pool (make-instance 'max-pool :max 3))
           (thread-count 100))
       (let* ((promises (loop for i from 0 below thread-count
                              collect (make-thread
                                       (lambda ()
                                         (bt:with-lock-held (*lock*)
                                           (incf *ctr*))
                                         (error "foobar"))
                                       :pool max-pool)))
              (threads (loop for promise in promises
                             collect (lparallel:force promise))))
         (dolist (thread threads)
           (bt:join-thread thread))
         (is (eql thread-count *ctr*)))))))
