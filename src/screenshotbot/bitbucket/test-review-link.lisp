;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/bitbucket/test-review-link
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/pro/bitbucket/plugin
                #:bitbucket-repo)
  (:import-from #:screenshotbot/dashboard/review-link
                #:get-canonical-pull-request-url))
(in-package :screenshotbot/bitbucket/test-review-link)

(util/fiveam:def-suite)

(test bitbucket-review-link
  (let ((repo (make-instance 'bitbucket-repo
                             :link "git@bitbucket.org:tdrhq/fast-example.git")))
    (is (equal "https://bitbucket.org/tdrhq/fast-example/pull-requests/2"
               (get-canonical-pull-request-url
                repo 2)))))
