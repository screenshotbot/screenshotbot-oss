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
                #:pull-request-with-url
                #:github-maybe-update-pull-request)
  (:import-from #:util/store
                #:with-test-store))

(util/fiveam:def-suite)

(test github-maybe-update-pull-request ()
  (with-test-store ()
   (with-open-file (s (path:catfile
                       #.(asdf:system-source-directory :screenshotbot)
                       "github/pull-request-example-1.json"))
     (let ((json (json:decode-json s))
           (random-url (generate-api-secret)))
       ;; but before we process this, let's use a random string in the
       ;; url field.
       (is-true random-url)
       (setf (assoc-value (assoc-value json :pull--request) :url)
             random-url)
       (github-maybe-update-pull-request json)
       (is-true (pull-request-with-url random-url))
       (let ((obj (pull-request-with-url random-url)))
         (is (equal
              "d87dea9a36924ca17f054d9586737187b82a8bd6"
              (pull-request-head obj)))
         (is (equal
              "458afc8769c2186d669a1cc7bbd64480172c49a5"
              (pull-request-base obj)))
         (is (equal
              "tdrhq/fast-example"
              (repo-full-name obj))))))))

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
