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
                #:make-recorder-run
                #:recorder-run))
(in-package :screenshotbot/bitbucket/test-plugin)


(util/fiveam:def-suite)

(test describe-pull-request-for-bitbucket
  (with-test-store ()
    (let ((repo (make-instance 'bitbucket-repo)))
      (is (equal "Pull Request"
                 (describe-pull-request repo (make-recorder-run
                                              :pull-request "https://google.com"))))
      (is (equal "Pull 2"
                 (describe-pull-request repo
                                        (make-recorder-run
                                         :pull-request "https://bitbucket.org/tdrhq/fast-example/pull-requests/2")))))))

(test describe-pull-request-for-bitbucket-for-run-with-branch
  (with-test-store ()
    (let ((repo (make-instance 'bitbucket-repo)))
      (is (equal "Pull Request"
                 (describe-pull-request repo (make-recorder-run
                                              :work-branch "foobar"
                                              :pull-request "https://google.com"))))
      (is (equal "Pull 2 (foobar)"
                 (describe-pull-request repo
                                        (make-recorder-run
                                         :work-branch "foobar"
                                         :pull-request "https://bitbucket.org/tdrhq/fast-example/pull-requests/2")))))))

(test describe-pull-request-for-bitbucket-for-empty-string-branch
  (with-test-store ()
    (let ((repo (make-instance 'bitbucket-repo)))
      (is (equal "Pull Request"
                 (describe-pull-request repo (make-recorder-run
                                              :work-branch ""
                                              :pull-request "https://google.com"))))
      (is (equal "Pull 2"
                 (describe-pull-request repo
                                        (make-recorder-run
                                         :work-branch ""
                                         :pull-request "https://bitbucket.org/tdrhq/fast-example/pull-requests/2")))))))

(test describe-pull-request-for-bitbucket-for-branch-with-/-in-name
  (with-test-store ()
    (let ((repo (make-instance 'bitbucket-repo)))
      (is (equal "Pull Request"
                 (describe-pull-request repo (make-recorder-run
                                              :work-branch ""
                                              :pull-request "https://google.com"))))
      (is (equal "Pull 2 (blah)"
                 (describe-pull-request repo
                                        (make-recorder-run
                                         :work-branch "feature/blah"
                                         :pull-request "https://bitbucket.org/tdrhq/fast-example/pull-requests/2")))))))
