;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/main
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/help
                #:help)
  (:import-from #:screenshotbot/sdk/sdk
                #:chdir-for-bin)
  (:import-from #:util/threading
                #:with-tags
                #:maybe-log-sentry
                #:*warning-count*
                #:with-extras
                #:*extras*
                #:funcall-with-sentry-logs)
  (:import-from #:screenshotbot/sdk/version-check
                #:*client-version*)
  (:import-from #:util/health-check
                #:def-health-check
                #:run-health-checks)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:screenshotbot/sdk/failed-run
                #:mark-failed)
  (:import-from #:screenshotbot/sdk/unchanged-run
                #:mark-unchanged-run)
  (:import-from #:screenshotbot/sdk/finalized-commit
                #:finalize-commit)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-feature-enabled-p
                #:remote-version
                #:api-context)
  (:import-from #:screenshotbot/sdk/hostname
                #:api-hostname)
  (:import-from #:screenshotbot/sdk/env
                #:make-env-reader)
  (:import-from #:screenshotbot/sdk/common-flags
                #:define-flag)
  (:import-from #:com.google.flag
                #:parse-command-line)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:e #:screenshotbot/sdk/env)
                    (#:sdk #:screenshotbot/sdk/sdk)
                    (#:static #:screenshotbot/sdk/static)
                    (#:firebase #:screenshotbot/sdk/firebase))
  ;; TODO: delete
  #+lispworks
  (:import-from #:screenshotbot/sdk/common-flags
                #:*api-key*
                #:*hostname*
                #:*api-secret*)
  (:import-from #:screenshotbot/sdk/cli-common
                #:root/command)
  (:import-from #:screenshotbot/sdk/sentry
                #:with-sentry)
  (:import-from #:util/request
                #:engine)
  (:import-from #:util/reused-ssl
                #:with-reused-ssl)
  (:import-from #:screenshotbot/sdk/xcresult
                #:make-xcresults-bundle
                #:xcresults-attachment-bundle)
  (:import-from #:screenshotbot/sdk/bundle
                #:close-bundle)
  (:import-from #:screenshotbot/sdk/server-log-appender
                #:flush-server-appender
                #:make-server-log-appender)
  (:export
   #:main))

(in-package :screenshotbot/sdk/main)

(define-flag *api-key*
  :selector "api-key"
  :default-value nil
  :type (or null string)
  :help "Screenshotbot API Key. Defaults to $SCREENSHOTBOT_API_KEY.")

(define-flag *api-secret*
  :selector "api-secret"
  :default-value nil
  :type (or null string)
  :help "Screenshotbot API Secret. Defaults to $SCREENSHOTBOT_API_SECRET")

(define-flag *hostname*
  :selector "api-hostname"
  :default-value ""
  :type string
  :help "Screenshotbot API Endpoint"
  :documentation "Only used for Enterprise or Open Source users, Defaults to `https://api.screenshotbot.io` or $SCREENSHOTBOT_API_HOSTNAME")


(def-easy-macro with-defaults (&binding api-context &fn fn)
  (sdk:parse-org-defaults)
  (let ((api-context (make-api-context)))
    (with-reused-ssl ((engine api-context))
      (let ((version (remote-version api-context)))
        (log:debug "Remote version is ~a" version))
      (let ((server-appender (make-server-log-appender api-context)))
        (when (api-feature-enabled-p api-context :server-cli-logs)
          (log4cl:add-appender log4cl:*root-logger*
                               server-appender))
        (unwind-protect
            (funcall fn api-context)
          (when (api-feature-enabled-p api-context :server-cli-logs)
            (flush-server-appender server-appender)))))))


(defun emptify (s)
  "If the string is empty, return nil"
  (if (str:emptyp s) nil s))

(defvar *api-key-info* "You can also use $SCREENSHOTBOT_API_KEY and $SCREENSHOTBOT_API_SECRET environment variable in CI.")

(defun make-api-context (&key (env (make-env-reader)))
  (let ((key (or (emptify *api-key*)
                 (e:api-key env)))
        (secret (or (emptify *api-secret*)
                    (e:api-secret env))))
    (when (str:emptyp key)
      (error "No --api-key provided. ~a" *api-key-info* ))
    (when(str:emptyp secret)
      (error "No --api-secret provided. ~a" *api-key-info*))
    (make-instance 'api-context
                   :key key
                   :secret secret
                   :hostname (or (emptify *hostname*)
                                 (emptify (e:api-hostname env))))))

(defun try-clingon (argv)
  (clingon:run (root/command) (cdr argv)))

(defun warn-when-obsolete-flags ()
  (macrolet ((test (flag)
               `(when ,flag
                  (warn "obsolete flag used: ~a ~a"
                         ',flag ,flag))))
    (test flags:*ios-multi-dir*)
    (test flags:*branch*)
    (test flags:*lang-regex*)
    (test flags:*device-regex*)
    (test flags:*ios-diff-dir*)))

(defun maybe-setup-ssl ()
  #+ (and lispworks linux)
  (let* ((potential-paths (list "/usr/lib/libssl.so.3"
                                "/usr/lib/libssl.so.1.1"))
         (path (loop for path in potential-paths
                     if (path:-e path)
                       return path)))
    (when path
      (handler-case
          (comm:ensure-ssl)
        (error ()
          (log:warn "Default libssl paths failed, trying ~a" path)
          (comm:ensure-ssl :library-path path))))))

(defun %main (&optional (argv #+lispworks system:*line-arguments-list*
                              #-lispworks (uiop:raw-command-line-arguments)))
  ;; We used to do :immediate-flush, but it looks like that might be
  ;; causing issues with Gradle. See T1514.
  (log:config :sane :pattern "[%D{%H:%M:%S}] %5p: %m%n")
  (log:config :info)

  (log:info "Screenshotbot SDK v~a" *client-version*)

  (maybe-setup-ssl)

  (let ((unrecognized  (parse-command-line (cdr argv))))
    (when flags:*verbose*
      (log:config :debug :oneline))
    (log:debug "Run this in interactive shell: ~S"
               `(progn
                  (chdir-for-bin ,(uiop:getcwd))
                  (%main ',argv)))

    (warn-when-obsolete-flags)
    (cond
      (unrecognized
       (cond
         ((or
           (not (str:emptyp (uiop:getenv "SCREENSHOTBOT_CLI_V2")))
           (str:s-member argv "dev")
           (str:s-member argv "ci")
           (str:s-member argv "batch")
           (str:s-member argv "download-run")
           #+nil ;; References "ci record" which doesn't exit
           (str:s-member argv "help"))
          (log:debug "Enabling V2 of Screenshotbot CLI interface")
          (try-clingon argv))
         (t
           (format t "Unrecognized arguments: ~a~%" (Str:join " " unrecognized))
           (help)
           (uiop:quit 1))))
      (flags:*help*
       (help))
      (flags:*versionp*
       ;; We've already printed the version by this point
       nil)
      (flags:*self-test*
       (uiop:quit (if (run-health-checks) 0 1)))
      (flags:*mark-failed*
       (with-defaults (api-context)
         (mark-failed api-context)))
      (flags:*unchanged-from*
       (with-defaults (api-context)
         (mark-unchanged-run api-context)))
      (flags:*static-website*
       (with-defaults (api-context)
         (static:record-static-website api-context flags:*static-website*
                                       :production flags:*production*
                                       :channel flags:*channel*
                                       :repo-url flags:*repo-url*
                                       :browser-configs flags:*browser-configs*
                                       :main-branch flags:*main-branch*
                                       :assets-root flags:*static-website-assets-root*)))
      (flags:*firebase-output*
       (firebase:with-firebase-output (flags:*firebase-output*)
         (with-defaults (api-context)
           (sdk:run-prepare-directory-toplevel api-context))))
      (flags:*finalize*
       (with-defaults (api-context)
         (finalize-commit api-context)))
      (flags:*xcresult*
       (when flags:*directory*
         (log:warn "Ignoring --directory since xcresult is provided"))
       (with-defaults (api-context)
         (let ((directory (make-xcresults-bundle flags:*xcresult*)))
           (prog1
               (sdk:single-directory-run api-context directory :channel flags:*channel*)
             (close-bundle directory)))))
      (flags:*directory*
       (with-defaults (api-context)
         (sdk:run-prepare-directory-toplevel api-context)))
      (t
       (help)
       (warn "No --directory provided")
       (uiop:quit 1)))))

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

#+nil ;; too noisy, and less important
(def-health-check sentry-logger-for-warnings ()
  (let ((out-stream (make-string-output-stream)))
    (with-sentry (:dry-run t)
      (warn "warning health-check for sdk"))))

#+lispworks
(lw:defadvice (clingon.utils:exit clingon-exit-warning :before) (&optional (code 0))
  (unless (= code 0)
    (warn "Exiting with code: ~a~%" code))
  (finish-output *error-output*))
