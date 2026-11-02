;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/sentry
  (:use #:cl)
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
  (:import-from #:screenshotbot/sdk/api-context
                #:*crash-api-context*
                #:cached-remote-version
                #:remote-version
                #:api-context)
  (:import-from #:screenshotbot/sdk/hostname
                #:format-api-url
                #:api-hostname)
  (:import-from #:sentry-client
                #:client-capture-exception
                #:encode-exception-event)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:util/reused-ssl
                #:with-reused-ssl)
  (:import-from #:screenshotbot/sdk/env
                #:make-env-reader)
  (:import-from #:screenshotbot/sdk/common-flags
                #:define-flag)
  (:import-from #:com.google.flag
                #:parse-command-line)
  (:local-nicknames (#:a #:alexandria)
                    (#:flags #:screenshotbot/sdk/flags)
                    (#:e #:screenshotbot/sdk/env)
                    (#:api-context #:screenshotbot/sdk/api-context)
                    (#:sdk #:screenshotbot/sdk/sdk))
  ;; TODO: delete
  #+lispworks
  (:import-from #:screenshotbot/sdk/common-flags
                #:*api-key*
                #:*hostname*
                #:*api-secret*)
  (:import-from #:core/cli/sentry
                #:default-sentry-output-stream
                #:with-cli-sentry))
(in-package :screenshotbot/sdk/sentry)

(defun post-crash (event)
  "Send the encoded Sentry event to /api/crash on the server we're
talking to."
  (unless (equal "true" (uiop:getenv "TDRHQ_IGNORE_SENTRY"))
    (let ((api-context (or *crash-api-context*
                           (make-instance 'api-context))))
      (with-reused-ssl ((api-context:engine api-context))
        (http-request
         (format-api-url api-context "/api/crash")
         :method :post
         :basic-authorization (unless (str:emptyp (api-context:key api-context))
                                (list (api-context:key api-context)
                                      (api-context:secret api-context)))
         :content event
         :content-type "application/json"
         :external-format-out :utf-8
         :want-string t
         :connection-timeout 10
         :read-timeout 10
         :engine (api-context:engine api-context))))))

(defun crash-api-unavailable-p ()
  "Whether we know the server doesn't support /api/crash. We can't call
REMOTE-VERSION here, since we might be reporting a crash from fetching
the version, so we only look at the version if we already have it."
  (let ((version (when *crash-api-context*
                   (cached-remote-version *crash-api-context*))))
    (and version (< version 22))))

#-screenshotbot-oss
(defclass api-crash-client (sentry:delivered-client)
  ()
  (:documentation "Reports crashes via the Screenshotbot server, which forwards them to
Sentry. If the server doesn't support that, reports directly to Sentry."))

#-screenshotbot-oss
(defmethod client-capture-exception ((self api-crash-client) condition &rest args &key tags extras)
  (declare (ignore args))
  ;; This might get called because the server is unavailable, and a
  ;; warning gets reported the same way as a crash, so a failure here
  ;; must not break the CLI.
  (handler-case
      (if (crash-api-unavailable-p)
          (call-next-method)
          (post-crash (encode-exception-event condition self :tags tags :extras extras)))
    (error (e)
      (log:debug "Could not report crash: ~a" e))))

(defun initialize-crash-client ()
  #-screenshotbot-oss
  (sentry-client:initialize-sentry-client
   "https://b0c00f1b8a0e499fbdba1de5013fc97b:0632f4ab8db64d55accd4b81c93e7b79@sentry.io/1460546"
   :client-class 'api-crash-client))

(def-easy-macro with-sentry (&key (on-error (lambda ()
                                              (uiop:quit 1)))
                                  (stream (default-sentry-output-stream))
                                  (dry-run nil)
                                  &fn fn)
  (with-tags (("cli-client" "true")
              #+lispworks ("api_hostname" *hostname*)
              #+lispworks ("api_id"  *api-key*)
              #+lispworks ("channel" flags:*channel*)
              ("cli-version" *client-version*))
    (with-extras (("features" *features*)
                  ("build_creator"
                   (uiop:getenv "BUILDKITE_BUILD_CREATOR"))
                  ("build-url" flags:*build-url*))
      (with-cli-sentry (:verbose flags:*verbose* :dry-run dry-run
                        :stream stream
                        :on-error on-error)
        (funcall fn)))))
