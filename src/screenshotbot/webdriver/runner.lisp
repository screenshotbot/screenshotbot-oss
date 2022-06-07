;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webdriver/runner
  (:use #:cl
        #:util/java)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:*runner*
   #:runner
   #:driver
   #:runner-take-screenshot
   #:finalize-runner))
(in-package :screenshotbot/webdriver/runner)

(named-readtables:in-readtable java-syntax)

(defvar *runner*)

(defun output-type-file ()
  (read-java-field #,org.openqa.selenium.OutputType "FILE"))

(defclass runner ()
  ((driver :initarg :driver
           :reader driver)))

(defmethod runner-take-screenshot ((runner runner) output)
  (let* ((driver (driver runner))
         (file (#_getScreenshotAs driver (output-type-file))))
    (ensure-directories-exist output)
    (uiop:rename-file-overwriting-target
     (#_getAbsolutePath file)
     output)))

(defmethod finalize-runner ((runner runner))
  (values))
