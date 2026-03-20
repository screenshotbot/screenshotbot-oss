;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/android/text-view
  (:use #:cl)
  (:import-from #:screenshotbot/android/api
                #:context-delegate)
  (:import-from #:screenshotbot/android/view
                #:def-view
                #:view
                #:delegate))
(in-package :screenshotbot/android/text-view)

(def-view text-view (view) "android.widget.TextView"
  ())

(lw-ji:define-java-callers "android.widget.TextView"
  (%set-text "setText"))

(defmethod initialize-instance :after ((self text-view) &key context text &allow-other-keys)
  (when text
    (set-text self text)))

(defmethod set-text ((self text-view) text)
  (%set-text (delegate self)
             text))

;;(%new-text-view (context-delegate screenshotbot/android/activity::*current-activity*))




