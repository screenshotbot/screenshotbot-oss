;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/test-review-link
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/gitlab/repo
                #:gitlab-repo)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/gitlab/review-link
                #:gitlab-review-link))
(in-package :screenshotbot/gitlab/test-review-link)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((repo (make-instance 'gitlab-repo
                               :link "https://gitlab.com/tdrhq/bar/car.git"))
          (run (make-recorder-run
                :gitlab-merge-request-iid 123)))
      (&body))))

(test review-link-is-normalized
  (with-fixture state ()
    (is (equal "https://gitlab.com/tdrhq/bar/car/-/merge_requests/123"
               (gitlab-review-link repo run)))))
