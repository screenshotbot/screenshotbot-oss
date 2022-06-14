;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/main
  (:use #:cl
        #:com.google.flag)
  (:import-from #:screenshotbot/sdk/help
                #:help)
  (:import-from #:screenshotbot/sdk/sdk
                #:chdir-for-bin)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:static #:screenshotbot/sdk/static))
  (:export
   #:main))
(in-package :screenshotbot/sdk/main)

(defun %main (&optional (argv #+lispworks system:*line-arguments-list*
                              #-lispworks nil))
  (log4cl:reset-logging-configuration)
  (log:config :info)
  (log:info "Screenshotbot SDK v2.3.13")
  (let ((unrecognized   (parse-command-line (cdr (command-line)))))
    (when flags:*verbose*
      (log:config :debug))
    (log:debug "Run this in interactive shell: ~S"
               `(progn
                  (chdir-for-bin ,(uiop:getcwd))
                  (%main ',argv)))
    (cond
      (unrecognized
       (format t "Unrecognized arguments: ~a~%" (Str:join " " unrecognized))
       (help))
      (flags:*help*
       (help))
      (flags:*ios-multi-dir*
       (sdk:parse-org-defaults)
       (sdk:run-ios-multi-dir-toplevel))
      (flags:*static-website*
       (sdk:parse-org-defaults)
       (static:record-static-website flags:*static-website*))
      (t
       (sdk:parse-org-defaults)
       (sdk:run-prepare-directory-toplevel)))))

(defun main (&rest args)
  (let ((error-handler (lambda (e)
                         (format t "~%~a~%~%" e)
                         #+lispworks
                         (dbg:output-backtrace (if flags:*verbose* :bug-form :brief))
                         #-lispworks
                         (trivial-backtrace:print-backtrace einteg)
                         (uiop:quit 1))))
   (handler-bind ((warning (lambda (warning)
                             (let ((msg (princ-to-string warning)))
                               ;; This warning is not very actionable
                               ;; for end-users, so let's muffle it
                               #+lispworks
                               (when (str:containsp "output-wait is not implemented" msg)
                                 (muffle-warning warning)))))
                  #+lispworks
                  (error error-handler))
     (apply '%main args)))
  #-sbcl
  (log4cl::exit-hook)
  (uiop:quit 0))
