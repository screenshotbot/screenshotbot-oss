;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :screenshotbot/android/activity
  (:use #:cl)
  (:import-from :screenshotbot/android/api
                :context-delegate)
  (:import-from #:screenshotbot/android/view
                #:delegate
                #:view)
  (:import-from #:screenshotbot/android/text-view
                #:text-view)
  (:export
   #:set-current-activity))
(in-package :screenshotbot/android/activity)

(defvar *current-activity*)

(defclass activity ()
  ((delegate :initarg :delegate
             :reader delegate
             :reader context-delegate)))

(lw-ji:define-java-callers "android.app.Activity"
  (%set-content-view "setContentView"))

(defmethod set-content-view ((self activity)
                             (view view))
  (%set-content-view
   (delegate self)
   (delegate view)))

(defun set-current-activity (activity)
  (setf *current-activity* (make-instance 'activity :delegate activity))
  nil)

;; (hcl:android-funcall-in-main-thread (lambda () (set-content-view *current-activity* (make-instance 'text-view :context *current-activity*))))

