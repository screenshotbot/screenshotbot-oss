;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/android/linear-layout
  (:use #:cl)
  (:import-from #:screenshotbot/android/view
                #:delegate
                #:view-group
                #:def-view))
(in-package :screenshotbot/android/linear-layout)


(def-view linear-layout (view-group) "android.widget.LinearLayout"
  ())

(lw-ji:define-java-callers "android.widget.LinearLayout"
  (%set-orientation "setOrientation"))

(defmethod set-orientation ((self linear-layout)
                            orientation)
  (%set-orientation
   (delegate self)
   (ecase orientation
     (:horizontal
      0)
     (:vertical
      1))))
