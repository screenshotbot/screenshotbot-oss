;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/android/edit-text
  (:use #:cl)
  (:import-from #:screenshotbot/android/text-view
                #:text-view)
  (:import-from #:screenshotbot/android/view
                #:def-view))
(in-package :screenshotbot/android/edit-text)

(def-view edit-text (text-view) "android.widget.EditText"
  ())

