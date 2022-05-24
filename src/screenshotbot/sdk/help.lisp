;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/help
  (:use #:cl
        #:alexandria)
  (:export
   #:help))
(in-package :screenshotbot/sdk/help)


(defun help ()
  (format t "Screenshotbot Recorder script~%~%")
  (format t "Usage: recorder [options]~%~%")
  (format t "Use this script from your CI pipelines or locally to
upload screenshots and generate reports~%~%")
  (format t "Options:~%")
  (loop for (name . flag) in com.google.flag::*registered-flags* do
    (let ((lines (str:lines (com.google.flag::help flag))))
      (format t "--~25A~40A~%" name
              (or (car lines) ""))
      (loop for l in (cdr lines) do
        (format t "~27A~A~%" " " (str:trim l)))))
  (format t "~%Copyright 2020-2021 Modern Interpreters Inc.~%")
  (format t "Please reach out to support@screenshotbot.io for any questions~%"))
