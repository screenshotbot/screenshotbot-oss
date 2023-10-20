;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-env
  (:use #:cl
        #:fiveam
        #:screenshotbot/sdk/env)
  (:import-from #:screenshotbot/sdk/env
                #:*all-readers*
                #:gitlab-ci-env-reader
                #:github-actions-env-reader
                #:bitbucket-pipeline-env-reader
                #:buildkite-env-reader
                #:azure-env-reader
                #:validp
                #:netlify-env-reader
                #:bitrise-env-reader
                #:circleci-env-reader
                #:env-reader)
  (:import-from #:screenshotbot/sdk/git
                #:git-message
                #:git-repo)
  (:import-from #:cl-mock
                #:if-called
                #:answer))
(in-package :screenshotbot/sdk/test-env)

(util/fiveam:def-suite)

(defun test-happy-fns (env-reader)
  (is (member (type-of env-reader)
              (list*
               'env-reader
               *all-readers*)))
  (validp env-reader)
  (api-key env-reader)
  (api-secret env-reader)
  (api-hostname env-reader)
  (pull-request-url env-reader)
  (sha1 env-reader)
  (build-url env-reader)
  (guess-channel-name env-reader)
  (repo-url env-reader)
  (work-branch env-reader))

(def-fixture state ()
  (&body))

(test base-reader
  (finishes (test-happy-fns (make-instance 'env-reader :overrides nil))))

(test circleci
  (finishes (test-happy-fns (make-instance 'circleci-env-reader
                                           :overrides nil))))

(test bitrise
  (finishes (test-happy-fns (make-instance 'bitrise-env-reader
                                           :overrides nil))))

(test netlify
  (finishes (test-happy-fns (make-instance 'netlify-env-reader
                                           :overrides nil))))

(test bitrise-pull-request-url
  (is (equal "https://bitbucket.com/fast-example/pull-requests/2"
             (pull-request-url
              (make-instance 'bitrise-env-reader
                             :overrides `(("GIT_REPOSITORY_URL" . "https://bitbucket.com/fast-example")
                                          ("BITRISE_PULL_REQUEST" . "2")))))))

(test azure
  (finishes (test-happy-fns (make-instance 'azure-env-reader
                                           :overrides nil))))

(test buildkite
  (finishes (test-happy-fns (make-instance 'buildkite-env-reader
                                           :overrides nil))))

(test bitbucket-pipeline
  (finishes (test-happy-fns (make-instance 'bitbucket-pipeline-env-reader
                                           :overrides nil))))

(test gitlab-ci-pipeline
  (finishes (test-happy-fns (make-instance 'gitlab-ci-env-reader
                                           :overrides nil))))

(test github-actions
  (finishes (test-happy-fns (make-instance 'github-actions-env-reader
                                           :overrides nil))))



(test buildkite-pull-request-is-not-nil-when-not-false
  (let ((one (make-instance
              'buildkite-env-reader
              :overrides `(("BUILDKITE_PULL_REQUEST" . "2")
                           ("BUILDKITE_REPO" . "https://github.com/tdrhq/fast-example")))))
    (is (equal "https://github.com/tdrhq/fast-example/pull/2"
               (pull-request-url one)))))

(test buildkite-pull-request-is-nil-when-false
  (let ((one (make-instance
              'buildkite-env-reader
              :overrides `(("BUILDKITE_PULL_REQUEST" . "false")
                           ("BUILDKITE_REPO" . "https://github.com/tdrhq/fast-example")))))
    (is (equal nil
               (pull-request-url one)))))

(test make-env-reader-happy-path
  (finishes (make-env-reader)))

(test all-readers-has-valid-types
  (loop for reader-name in *all-readers*
        do (is-true (find-class reader-name))))

(test github-reads-env-from-git-repo
  (cl-mock:with-mocks ()
    (if-called 'git-message
               (lambda (git-repo)
                 " Merge 02520edac7d38b71bacaee1c32d3c7f5cd880f8b into 38181385c139952159a3cf69950f8ff658395efb  "))
    (let ((reader (make-instance 'github-actions-env-reader
                                 :overrides `(("GITHUB_EVENT_NAME" . "pull_request")
                                              ("GITHUB_SHA" . "bleh")))))
      (is (equal "02520edac7d38b71bacaee1c32d3c7f5cd880f8b" (sha1 reader))))))

(test github-reads-env-when-not-pull-request
  (cl-mock:with-mocks ()
    (if-called 'git-message
               (lambda (git-repo)
                 " Merge 02520edac7d38b71bacaee1c32d3c7f5cd880f8b into 38181385c139952159a3cf69950f8ff658395efb  "))
    (let ((reader (make-instance 'github-actions-env-reader
                                 :overrides `(("GITHUB_EVENT_NAME" . "push")
                                              ("GITHUB_SHA" . "bleh")))))
      (is (equal "bleh" (sha1 reader)))
      (is-false (pull-request-url reader)))))

(test github-reads-pull-request-correctly
  (cl-mock:with-mocks ()
    (let ((reader (make-instance 'github-actions-env-reader
                                 :overrides `(("GITHUB_EVENT_NAME" . "pull_request")
                                              ("GITHUB_SERVER_URL" . "https://github.com")
                                              ("GITHUB_REPOSITORY" . "tdrhq/fast-example")
                                              ("GITHUB_REF" . "refs/pull/22/merge")))))
      (is (equal "https://github.com/tdrhq/fast-example/pull/22" (pull-request-url reader))))))
