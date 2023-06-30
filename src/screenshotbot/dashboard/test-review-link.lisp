;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-review-link
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/channel
                #:repo)
  (:import-from #:screenshotbot/dashboard/review-link
                #:describe-pull-request)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/git-repo
                #:generic-git-repo)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run))
(in-package :screenshotbot/dashboard/test-review-link)

(util/fiveam:def-suite)

(test describe-pull-request-url-for-branch
  (with-test-store ()
    (let ((repo (make-instance 'generic-git-repo)))
      (is (equal "Pull Request"
                 (describe-pull-request repo (make-recorder-run
                                              :work-branch ""
                                              :pull-request "https://google.com"))))
      (is (equal "Pull Request (blah)"
                 (describe-pull-request repo
                                        (make-recorder-run
                                         :work-branch "feature/blah"
                                         :pull-request "https://bitbucket.org/tdrhq/fast-example/pull-requests/2")))))))
