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
                #:with-extras
                #:*extras*
                #:funcall-with-sentry-logs)
  (:import-from #:screenshotbot/sdk/version-check
                #:*client-version*
                #:with-version-check)
  (:import-from #:util/health-check
                #:def-health-check
                #:run-health-checks)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/sdk/failed-run
                #:mark-failed)
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

  (log:info "Screenshotbot SDK v~a" *client-version*)
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
       (help)
       (uiop:quit 1))
      (flags:*help*
       (help))
      (flags:*versionp*
       ;; We've already printed the version by this point
       nil)
      (flags:*self-test*
       (uiop:quit (if (run-health-checks) 0 1)))
      (flags:*mark-failed*
       (sdk:parse-org-defaults)
       (with-version-check ()
         (mark-failed)))
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

(def-easy-macro with-sentry (&key (on-error (lambda ()
                                              (uiop:quit 1)))
                                  (dry-run nil)
                                  (stream
                                   #+lispworks
                                   (system:make-stderr-stream)
                                   #-lispworks
                                   *standard-output*)
                                  &fn fn)
  #-screenshotbot-oss
  (sentry-client:initialize-sentry-client
   sentry:*dsn* :client-class 'sentry:delivered-client)
  (with-extras (("api_hostname" flags:*hostname*)
                ("api_id"  flags:*api-key*)
                ("features" *features*)
                ("channel" flags:*channel*)
                ("build-url" flags:*build-url*)
                ("hostname" (uiop:hostname)))
   (let ((error-handler (lambda (e)
                          (format stream "~%~a~%~%" e)
                          #+lispworks
                          (dbg:output-backtrace (if flags:*verbose* :bug-form :brief)
                                                :stream stream)
                          #-lispworks
                          (trivial-backtrace:print-backtrace e stream)
                          (unless dry-run
                            #-screenshotbot-oss
                            (util/threading:log-sentry e))
                          (funcall on-error))))
     (handler-bind (#+lispworks
                    (error error-handler))
       (funcall fn)))))

(defun main (&rest args)
  (uiop:setup-command-line-arguments)

  (with-sentry ()
    (apply '%main args))

  #-sbcl
  (log4cl::exit-hook)
  (uiop:quit 0))

#-screenshotbot-oss
(def-health-check sentry-logger ()
  (let ((out-stream (make-string-output-stream)))
    (restart-case
        (with-sentry (:on-error (lambda ()
                                  (invoke-restart 'expected))
                      :stream out-stream
                      :dry-run t)
          (error "health-check for sdk"))
     (expected ()
       nil))
    (assert (cl-ppcre:scan ".*RUN-HEALTH-CHECKS.*"
                           (get-output-stream-string out-stream)))))
