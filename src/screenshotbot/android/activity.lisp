;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :screenshotbot/android/activity
  (:use #:cl)
  (:export
   #:set-current-activity))
(in-package :screenshotbot/android/activity)

(defvar *current-activity*)

(defun set-current-activity (activity)
  (setf *current-activity* activity)
  nil) 

