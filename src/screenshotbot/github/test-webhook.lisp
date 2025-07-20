;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/github/test-webhook
  (:use #:cl
        #:alexandria
        #:fiveam)
  (:import-from #:screenshotbot/model/core
                #:generate-api-secret)
  (:import-from #:screenshotbot/github/webhook
                #:pull-request
                #:pull-request-head
                #:pull-request-base
                #:repo-full-name
                #:github-get-canonical-repo
                #:pull-request-with-url)
  (:import-from #:util/store
                #:with-test-store))

(util/fiveam:def-suite)

(test github-get-cannonical-repo
  (is (equal "https://github.com/foo/bar"
             (github-get-canonical-repo "https://github.com/foo/bar.git")))
  (is (equal "https://github.com/foo/bar"
             ;; Github actions uses this syntax
             (github-get-canonical-repo "git://github.com/foo/bar.git")))
  (is (equal "https://github.com/foo/bar"
             (github-get-canonical-repo "https://api.github.com/foo/bar.git")))
  (is (equal "https://github.com/foo/bar"
             (github-get-canonical-repo "git@github.com:foo/bar.git")))
  (is (equal "https://github.com/foo/bar"
             (github-get-canonical-repo "ssh://git@github.com:foo/bar.git")))
  ;; This one is a hack to deal with people using an incorrect repo URL
  (is (equal "https://github.com/foo/bar"
             (github-get-canonical-repo "ssh://git@github.com/foo/bar.git"))))

(test github-get-canonical-repo-for-bitbucket
  (is (equal "https://bitbucket.org/tdrhq/fast-example"
             (github-get-canonical-repo
              "git@bitbucket.org:tdrhq/fast-example.git"))))
