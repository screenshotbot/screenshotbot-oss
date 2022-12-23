;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/gradle
  (:use #:cl))
(in-package :screenshotbot/sdk/gradle)

(defmacro def-ext-fun (name args &body body)
  `(progn
     (defun ,name ,args
       ,@body
       1)
     (lw:deliver-keep-symbol-names ',name)
     (lw:deliver-keep-symbols ',name)))

(lw-ji:define-java-callers "io.screenshotbot.plugin.ScreenshotbotPlugin"

  (%println "println"))

(defun println (msg &rest args)
  (%println (apply #'format nil msg args)))

(def-ext-fun record-facebook-task (adb)
  (println "Got adb ~A~%" adb))
