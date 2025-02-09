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

(defhandler (nil :uri "/test-timeout") ()
  "For testing timeout handling from the SDK"
  (when (equal "thecharmer" (uiop:hostname))
    (sleep 120)))



