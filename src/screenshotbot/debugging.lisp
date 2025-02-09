;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/debugging
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler))
(in-package :screenshotbot/debugging)

(defun enabledp ()
  (equal "thecharmer" (uiop:hostname)))

(defhandler (nil :uri "/test-timeout") ()
  "For testing timeout handling from the SDK"
  (when (enabledp)
    (sleep 120)))

(defhandler (nil :uri "/test-timeout-while-sending") ()
  "What happens if we timeout half-way through? Can we detect it?"
  (when (enabledp)
    (let ((stream (hunchentoot:send-headers)))
      (write-string "arnold" stream)
      (force-output stream)
      (sleep 30)
      (write-string "foobar" stream)
      (close stream))))

