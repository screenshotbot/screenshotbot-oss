;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-env
  (:use #:cl
        #:fiveam
        #:screenshotbot/sdk/env)
  (:import-from #:screenshotbot/sdk/env
                #:validp
                #:netlify-env-reader
                #:bitrise-env-reader
                #:circleci-env-reader
                #:env-reader))
(in-package :screenshotbot/sdk/test-env)

(util/fiveam:def-suite)

(defun test-happy-fns (env-reader)
  (validp env-reader)
  (api-key env-reader)
  (api-secret env-reader)
  (api-hostname env-reader)
  (pull-request-url env-reader)
  (sha1 env-reader)
  (build-url env-reader)
  (guess-channel-name env-reader)
  (repo-url env-reader))

(def-fixture state ()
  (&body))

(test base-reader
  (finishes (test-happy-fns (make-instance 'env-reader))))

(test circleci
  (finishes (test-happy-fns (make-instance 'circleci-env-reader))))

(test bitrise
  (finishes (test-happy-fns (make-instance 'bitrise-env-reader))))

(test netlify
  (finishes (test-happy-fns (make-instance 'netlify-env-reader))))

(test bitrise-pull-request-url
  (is (equal "https://bitbucket.com/fast-example/pull-requests/2"
             (pull-request-url
              (make-instance 'bitrise-env-reader
                             :overrides `(("GIT_REPOSITORY_URL" . "https://bitbucket.com/fast-example")
                                          ("BITRISE_PULL_REQUEST" . "2")))))))
