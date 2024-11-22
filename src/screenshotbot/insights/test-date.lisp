;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/test-date
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/insights/date
                #:list-dates
                #:increment-date
                #:%date-to-universal
                #:format-date))
(in-package :screenshotbot/insights/test-date)


(util/fiveam:def-suite)

(test simple-format
  (is (equal "1900-01-01"
             (format-date 10))))

(test %date-to-universal
  (is (equal 0 (%date-to-universal "1900-01-01")))
  (is (equal 86400 (%date-to-universal "1900-01-02"))))

(test increment-date
  (is (equal "1900-01-02" (increment-date "1900-01-01")))
  (is (equal "2024-02-01" (increment-date "2024-01-31"))))

(test list-dates
  (is (equal
       (list "2024-01-31"
             "2024-02-01"
             "2024-02-02")
       (list-dates
        :from "2024-01-31"
        :to "2024-02-02"))))

