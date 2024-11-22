;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/test-date
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/insights/date
                #:format-date))
(in-package :screenshotbot/insights/test-date)


(util/fiveam:def-suite)

(test simple-format
  (is (equal "1900-01-01"
             (format-date 10))))

