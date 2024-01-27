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
                #:remote-version
                #:api-context
                #:desktop-api-context)
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

(def-easy-macro with-sentry (&key (on-error (lambda ()
                                              (uiop:quit 1)))
                                  (stream (default-sentry-output-stream))
                                  (dry-run nil)
                                  &fn fn)
  #-screenshotbot-oss
  (sentry-client:initialize-sentry-client
   sentry:*dsn* :client-class 'sentry:delivered-client)
  (with-tags (("cli-client" "true")
              ("api_hostname" *hostname*)
              ("api_id"  *api-key*)
              ("channel" flags:*channel*))
    (with-extras (("api_id"  *api-key*)
                  ("features" *features*)
                  ("build_creator"
                   (uiop:getenv "BUILDKITE_BUILD_CREATOR"))
                  ("cli-version" *client-version*)
                  ("build-url" flags:*build-url*)
                  #+lispworks
                  ("cmd-line-trimmed"
                   (mapcar
                    #'trim-arg
                    sys:*line-arguments-list*))
                  ("hostname" (uiop:hostname))
                  #+lispworks
                  ("openssl-version" (comm:openssl-version)))


      (with-cli-sentry (:verbose flags:*verbose* :dry-run dry-run
                        :stream stream
                        :on-error on-error)
        (funcall fn)))))
