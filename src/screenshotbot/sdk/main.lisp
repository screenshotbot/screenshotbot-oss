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
  (:import-from #:util/threading
                #:*extras*
                #:funcall-with-sentry-logs)
  (:import-from #:screenshotbot/sdk/version-check
                #:with-version-check)
  (:import-from #:util/health-check
                #:run-health-checks)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:static #:screenshotbot/sdk/static)
                    (#:firebase #:screenshotbot/sdk/firebase))
  (:export
   #:main))
(in-package :screenshotbot/sdk/main)

(defun %main (&optional (argv #+lispworks system:*line-arguments-list*
                              #-lispworks (uiop:command-line-arguments)))
  (log:config :sane :immediate-flush t)
  (log:config :info)

  (log:info "Screenshotbot SDK v2.4.3")
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
      (flags:*self-test*
       (uiop:quit (if (run-health-checks) 0 1)))
      (flags:*ios-multi-dir*
       (sdk:parse-org-defaults)
       (sdk:run-ios-multi-dir-toplevel))
      (flags:*static-website*
       (sdk:parse-org-defaults)
       (with-version-check ()
        (static:record-static-website flags:*static-website*)))
      (flags:*firebase-output*
       (firebase:with-firebase-output (flags:*firebase-output*)
         (sdk:parse-org-defaults)
         (with-version-check ()
           (sdk:run-prepare-directory-toplevel))))
      (t
       (sdk:parse-org-defaults)
       (with-version-check ()
        (sdk:run-prepare-directory-toplevel))))))

(defun main (&rest args)
  (uiop:setup-command-line-arguments)
  #-screenshotbot-oss
  (sentry-client:initialize-sentry-client
   sentry:*dsn*)
  (let ((error-handler (lambda (e)
                         (format t "~%~a~%~%" e)
                         #+lispworks
                         (dbg:output-backtrace (if flags:*verbose* :bug-form :brief))
                         #-lispworks
                         (trivial-backtrace:print-backtrace einteg)
                         #-screenshotbot-oss
                         (util/threading:log-sentry e)
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
