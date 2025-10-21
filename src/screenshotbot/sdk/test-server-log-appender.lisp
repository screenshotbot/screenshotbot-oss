;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-server-log-appender
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/server-log-appender
                #:cli-log-stream))
(in-package :screenshotbot/sdk/test-server-log-appender)

(util/fiveam:def-suite)


(test simple-write-stuff
  (let ((stream (make-instance 'cli-log-stream :api-context nil)))
    (format stream "hello world~%")
    (finish-output stream)
    (pass)))
