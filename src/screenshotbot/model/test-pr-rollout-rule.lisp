;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-pr-rollout-rule
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/pr-rollout-rule
                #:disable-pull-request-checks-p
                #:whitelist-rule)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run))
(in-package :screenshotbot/model/test-pr-rollout-rule)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((rule (make-instance 'whitelist-rule
                               :emails '("foo@gmail.com" "bar@gmail.com"))))
      (&body))))


(test basic-run-without-info-is-blacklisted
  (with-fixture state ()
    (is-true
     (disable-pull-request-checks-p
      rule (make-recorder-run :screenshots nil)))
    (is-true
     (disable-pull-request-checks-p
      rule (make-recorder-run :screenshots nil
                              :author "arnold@example.com"
                              :work-branch "foo-bar")))))

(test with-branch-name-contains-screenshotbot
  (with-fixture state ()
    (is-false
     (disable-pull-request-checks-p
      rule (make-recorder-run :screenshots nil
                              :work-branch "foo-screenshotbot-bar")))))

(test with-branch-has-proper-author
  (with-fixture state ()
    (is-false
     (disable-pull-request-checks-p
      rule (make-recorder-run :screenshots nil
                              :author "foo@gmail.com"
                              :work-branch "foo-bar")))))
