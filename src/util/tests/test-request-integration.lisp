;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-request-integration
  (:use #:cl
        #:fiveam)
  (:import-from #:util/request
                #:http-request
                #:timeout-error))
(in-package :util/tests/test-request-integration)

(util/fiveam:def-suite)

;; AT TIME OF WRITING, these tests don't work. I'm putting it in here
;; to start building the infra for these.

(defparameter *enablep* nil
  "By default, we don't run these tests since they might be flaky and
expensive.")

(def-fixture state ()
  (when *enablep*
    (&body)))

(test timeout
  (with-fixture state ()
    (signals timeout-error
     (util/request:http-request
      "https://staging.screenshotbot.io/test-timeout"
      :read-timeout 5
      :write-timeout 5
      :connection-timeout 5))))

(test timeout-without-ssl
  (with-fixture state ()
    (multiple-value-bind (output err)
        (util/request:http-request
         "https://localhost:4001/test-timeout"
         :read-timeout 5
         :write-timeout 5
         :connection-timeout 5)
      (Error "here"))))


(test timeout-while-sending
  (with-fixture state ()
    (let ((res (http-request
                "https://staging.screenshotbot.io/test-timeout-while-sending"
                :read-timeout 5
                :write-timeout 5
                :connection-timeout 5)))
      (is (equal "arnoldfoobar" res)))))

(defvar +crlf+
  (format nil "~a~a" #\Return #\Linefeed))

#+lispworks
(test manual-test-for-lispworks-reproduction
  (with-fixture state ()
    (let ((stream (comm:open-tcp-stream "staging.screenshotbot.io" 443 :ssl-ctx t
                                                                       :direction :io
                                                                       :read-timeout 5)))
      (format stream "GET /test-timeout-while-sending HTTP/1.1")
      (format stream +crlf+)
      (format stream "Host: staging.screenshotbot.io")
      (format stream +crlf+)
      (format stream +crlf+)
      (force-output stream)
      (let ((lines 
              (loop for line = (read-line stream nil)
                    while line
                    collect line)))))))
