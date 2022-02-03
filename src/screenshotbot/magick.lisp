;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:run-magick))
(in-package :screenshotbot/magick)

(defun run-magick (command &rest args &key (error-output t) (output t)
                             (ignore-error-status nil))
  "Wrapper for magick commands, in the future we might run this in-process"
  (restart-case
      (let ((command (loop for str in command
                           if (pathnamep str)
                             collect (namestring str)
                           else
                             collect str)))

        (let ((prefix #-screenshotbot-oss (list "magick")))
          (uiop:run-program
           (append prefix command)
           :output output
           :error-output error-output
           :ignore-error-status ignore-error-status)))
    (retry-run-magick ()
      (apply #'run-magick command args))))
