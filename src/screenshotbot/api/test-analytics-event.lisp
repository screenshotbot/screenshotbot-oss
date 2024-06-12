;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :screenshotbot/api/test-analytics-event
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/analytics-event
                #:parse-all-events)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/api/test-analytics-event)


(util/fiveam:def-suite)

(test simple-analytics-event
  (with-fake-request ()
    (parse-all-events
     "[{\"foo\":\"bar\"}]" )))
