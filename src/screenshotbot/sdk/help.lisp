;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/sdk/help
  (:use #:cl
        #:alexandria)
  (:import-from #:screenshotbot/sdk/common-flags
                #:obsolete?)
  (:export
   #:help))
(in-package :screenshotbot/sdk/help)


(defun help ()
  (format t "Screenshotbot Recorder script~%~%")
  (format t "Usage: recorder [options]~%~%")
  (format t "Use this script from your CI pipelines or locally to
upload screenshots and generate reports~%~%")
  (format t "We are currently migrating the commands from a single top-level
command to multiple subcommands. Use `recorder help` to get documentation for subcommands.~%~%")
  (format t "Options:~%~%")
  (loop for (name . flag) in (sort com.google.flag::*registered-flags* #'string< :key #'car) do
    (let* ((lines (mapcar #'str:trim (str:lines (com.google.flag::help flag))))
           (lines (loop for line in lines
                        for start from 0
                        if (= start 0)
                          collect line
                        else
                          collect (str:concat " " line)))
           (lines (cond
                    ((< (length name) 22)
                     lines)
                    (t
                     (list* "" lines)))))
      (unless (obsolete? flag)
        (format t " --~22A~40A~%" name
                (or (car lines) ""))
        (loop for l in (cdr lines) do
          (format t "~25A~A~%" " " l)))))
  (format t "~%Copyright 2020-2022 Modern Interpreters Inc.~%")
  (format t "Please reach out to support@screenshotbot.io for any questions~%"))
