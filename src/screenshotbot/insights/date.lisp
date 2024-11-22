;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/date
  (:use #:cl)
  (:export
   #:format-date))
(in-package :screenshotbot/insights/date)

(defun %format-date (ts)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time ts 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defparameter *format-date-cache* (make-hash-table))

(defun format-date (ts)
  "format-date can be surprisingly slow, and is a bottleneck."
  (util:or-setf
   (gethash ts *format-date-cache*)
   (%format-date ts)))
