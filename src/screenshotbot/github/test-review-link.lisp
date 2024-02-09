;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/test-review-link
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/github/access-checks
                #:github-repo)
  (:import-from #:screenshotbot/dashboard/review-link
                #:get-canonical-pull-request-url))
(in-package :screenshotbot/github/test-review-link)


(util/fiveam:def-suite)

(test github-review-link
  (let ((repo (make-instance 'github-repo
                             :link "git@github.com:tdrhq/fast-example.git")))
    (is (equal "https://github.com/tdrhq/fast-example/pull/2"
               (get-canonical-pull-request-url
                repo 2)))))
