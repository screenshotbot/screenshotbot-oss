;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/github/test-access-checks
  (:use #:cl
        #:fiveam
        #:./access-checks
        #:../user-api))

(util/fiveam:def-suite)

#-sbcl ;; java
(test simple-creation
  (finishes
    (github-client)))


(test github-repo-commit-link
  (is (equal
       "https://github.com/tdrhq/web/commit/abcd12"
       (commit-link
        (make-instance 'github-repo
                        :link "https://github.com/tdrhq/web")
        "abcd12")))
  (is (equal
       "https://github.com/tdrhq/web/commit/abcd12"
       (commit-link
        (make-instance 'github-repo
                        :link "https://github.com/tdrhq/web.git")
        "abcd12")))
  (is (equal
       "https://github.com/tdrhq/web/commit/abcd12"
       (commit-link
        (make-instance 'github-repo
                        :link "git@github.com:tdrhq/web")
        "abcd12")))
  (is (equal
       "https://github.com/tdrhq/web/commit/abcd12"
       (commit-link
        (make-instance 'github-repo
                        :link "git@github.com:tdrhq/web.git")
        "abcd12"))))
