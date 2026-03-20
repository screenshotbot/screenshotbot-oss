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
                #:add-view
                #:delegate
                #:view)
  (:import-from #:screenshotbot/android/text-view
                #:text-view)
  (:import-from #:screenshotbot/android/edit-text
                #:edit-text)
  (:import-from #:screenshotbot/android/linear-layout
                #:set-orientation
                #:linear-layout)
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

(defun simple-test ()
  (hcl:android-funcall-in-main-thread
   (lambda ()
     (let ((view1 (make-instance 'text-view :context *current-activity*
                                            :text "foo"))
           (view2 (make-instance 'text-view :context *current-activity*
                                            :text "bar"))
           (username (make-instance 'edit-text :context *current-activity*
                                    :text "carbar"))
           (linear-layout (make-instance 'linear-layout :context *current-activity*)))
       (set-orientation linear-layout :vertical)
       (add-view linear-layout view1)
       (add-view linear-layout view2)
       (add-view linear-layout username)
      (set-content-view *current-activity* linear-layout)))))

;; (simple-test)



