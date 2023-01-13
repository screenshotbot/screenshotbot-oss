;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/bitbucket/test-plugin
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/review-link
                #:describe-pull-request)
  (:import-from #:screenshotbot/pro/bitbucket/plugin
                #:bitbucket-repo)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run))
(in-package :screenshotbot/bitbucket/test-plugin)


(util/fiveam:def-suite)

(test describe-pull-request-for-bitbucket
  (with-test-store ()
    (let ((repo (make-instance 'bitbucket-repo)))
      (is (equal "Pull Request"
                 (describe-pull-request repo (make-instance 'recorder-run
                                                            :pull-request-url "https://google.com"))))
      (is (equal "pull-requests/2"
                 (describe-pull-request repo
                                        (make-instance 'recorder-run
                                                       :pull-request "https://bitbucket.org/tdrhq/fast-example/pull-requests/2")))))))
