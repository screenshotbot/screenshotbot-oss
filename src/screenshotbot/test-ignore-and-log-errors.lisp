;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-ignore-and-log-errors
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/ignore-and-log-errors
                #:ignored-error-warning
                #:ignore-and-log-errors)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-ignore-and-log-errors)

(util/fiveam:def-suite)

(test ignore-and-log-errors
  (signals ignored-error-warning
    (ignore-and-log-errors ()
      (error "foobar")))
  (ignore-and-log-errors ()
    (error "foobar")))
