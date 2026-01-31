;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-commit-graph
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/integration-fixture
                #:with-sdk-integration)
  (:import-from #:screenshotbot/sdk/commit-graph
                #:all-remote-refs
                #:commit-graph-updater
                #+lispworks
                #:want-remote-ref
                #+lispworks
                #:update-from-pack
                #:new-flow-enabled-p
                #:update-commit-graph
                #:get-commit-graph-refs)
  (:import-from #:screenshotbot/git-repo
                #:commit-graph-dag
                #:commit-graph)
  (:import-from #:screenshotbot/model/commit-graph
                #:find-commit-graph)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:screenshotbot/sdk/git
                #:repo-link
                #:get-remote-url
                #:git-command
                #:fetch-remote-branch)
  (:import-from #:cl-mock
                #:answer
                #:if-called)
  #+lispworks
  (:import-from #:screenshotbot/sdk/git-pack
                #:local-upload-pack)
  (:import-from #:screenshotbot/api/core
                #:*wrap-internal-errors*)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context-prepared-p
                #:api-feature-enabled-p
                #:api-features)
  (:import-from #:fiveam-matchers/misc
                #:is-not-null)
  (:local-nicknames (#:dto #:screenshotbot/api/model)
                    (#:test-git #:screenshotbot/sdk/test-git)
                    (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/test-commit-graph)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
    (let ((*wrap-internal-errors* nil)
          (auto-restart:*global-enable-auto-retries-p* nil))
      (with-sdk-integration (api-context :company company)
        (gk:create :cli-shallow-clones)
        (let ((self (make-instance 'commit-graph-updater :api-context api-context)))
          (&body))))))

(defvar *repo* "https://github.com/tdrhq/fast-example.git")

(test happy-path-without-refs
  (with-fixture state ()
    (is (eql nil (get-commit-graph-refs
                  self
                  *repo*)))))

(test case-with-repo-though
  (with-fixture state ()
    (make-instance 'commit-graph
                   :company company
                   :url *repo*
                   :refs `(("master" . "abcd")))
    (assert-that
     (get-commit-graph-refs
      self
      *repo*)
     (contains
      (has-typep 'dto:git-ref)))))

(test update-commit-graph-happy-path
  (with-fixture state ()
   (if-called 'fetch-remote-branch
              (lambda (repo branch)
                (values)))
    (test-git:with-git-repo (repo :dir dir)
      (test-git:make-commit repo "foo")
      (test-git:make-commit repo "bar")
      (finishes
       (update-commit-graph self
                            repo
                            "main")))))

(test gk-is-propagated--validating-assumption
  (with-fixture state ()
    (gk:enable :cli-shallow-clones)
    (is-false (api-context-prepared-p api-context))
    (is-true (api-feature-enabled-p api-context :cli-shallow-clones))))

(test new-flow-enabled-p-happy-path
  (with-fixture state ()
    (gk:enable :cli-shallow-clones)
    (is-true (api-feature-enabled-p api-context :cli-shallow-clones))
    (test-git:with-git-repo (repo :dir dir)
      (is-false (new-flow-enabled-p self repo))
      (git::$ (git-command repo) "remote" "add" "origin" "git@github.com:tdrhq/fast-example.git")
      (#+lispworks is-true #-lispworks is-false (new-flow-enabled-p self repo)))))

#+lispworks
(test update-from-pack
  (with-fixture state ()
    (test-git:with-git-repo (repo :dir dir)
      (test-git:make-commit repo "foo")
      (test-git:enable-server-features repo)
      (let ((upload-pack (local-upload-pack repo)))
        (finishes
         (update-from-pack
          self
          upload-pack
          "git@github.com:tdrhq/fast-example.git"
          (list (git:current-branch repo))))

        (let* ((commit-graph (first (bknr.datastore:class-instances 'commit-graph)))
               (dag (commit-graph-dag commit-graph)))
          (assert-that (dag:get-commit dag (git:current-commit repo))
                       (is-not-null)))))))

#+lispworks
(test update-from-pack-twice
  (with-fixture state ()
    (test-git:with-git-repo (repo :dir dir)
      (test-git:make-commit repo "foo")
      (test-git:enable-server-features repo)
      (let ((upload-pack (local-upload-pack repo)))
        (update-from-pack
         self
         upload-pack
         "git@github.com:tdrhq/fast-example.git"
         (list (git:current-branch repo))))
      (test-git:make-commit repo "bar")
      (let ((upload-pack (local-upload-pack repo)))
        (finishes
         (update-from-pack
          self
          upload-pack
          "git@github.com:tdrhq/fast-example.git"
          (list (git:current-branch repo))))))))

#+lispworks
(test update-from-pack-twice-when-nothing-has-changed
  (with-fixture state ()
    (test-git:with-git-repo (repo :dir dir)
      (test-git:make-commit repo "foo")
      (test-git:make-commit repo "bar")
      (test-git:enable-server-features repo)
      (let ((upload-pack (local-upload-pack repo)))
        (update-from-pack
         self
         upload-pack
         "git@github.com:tdrhq/fast-example.git"
         (list (git:current-branch repo))))
      (let ((upload-pack (local-upload-pack repo)))
        (finishes
         (update-from-pack
          self
          upload-pack
          "git@github.com:tdrhq/fast-example.git"
          (list (git:current-branch repo))))))))

#+lispworks
(test new-flow-uses-repo-link-not-git-remote
  "When --repo-url is passed (e.g. GitHub URL), but the git remote points
elsewhere (e.g. GitLab mirror), the commit graph should be uploaded to
the --repo-url, not the git remote. See T2224."
  (with-fixture state ()
    (let ((github-url "https://github.com/example-org/my-project")
          (gitlab-url "https://gitlab-ci-token:glcbt@gitlab.example.com/internal/my-project.git"))
      ;; Create repo with :link set to GitHub URL (simulating --repo-url flag)
      (test-git:with-git-repo (repo :link github-url :dir dir)
        (test-git:make-commit repo "initial commit")
        (test-git:enable-server-features repo)
        ;; Set git remote to GitLab URL (simulating GitLab CI clone)
        (git::$ (git-command repo) "remote" "add" "origin" gitlab-url)
        ;; Verify the setup: repo-link is GitHub, git remote is GitLab
        (is (equal github-url (repo-link repo)))
        (is (equal gitlab-url (get-remote-url repo)))
        ;; Run the new flow
        (let ((upload-pack (local-upload-pack repo)))
          (update-from-pack
           self
           upload-pack
           ;; BUG: Currently this function is called with git:get-remote-url
           ;; but it should use git:repo-link
           github-url
           (list (git:current-branch repo))
           :current-commit (git:current-commit repo)))
        ;; Verify: commit graph should exist for GitHub URL
        (let ((github-commit-graph
                (find-commit-graph
                 company github-url)))
          (is-true github-commit-graph
                   "Commit graph should be created for --repo-url (GitHub)"))
        ;; Verify: commit graph should NOT exist for GitLab URL
        (let ((gitlab-commit-graph
                (find-commit-graph
                 company gitlab-url)))
          (is-false gitlab-commit-graph
                    "Commit graph should NOT be created for git remote (GitLab)"))))))

#+lispworks
(test update-commit-graph-new-style-uses-repo-link
  "Regression test for T2224: update-commit-graph-new-style should use
repo-link (--repo-url) not git:get-remote-url when identifying the repo
on the server. This test will FAIL until the bug is fixed."
  (with-fixture state ()
    (gk:enable :cli-shallow-clones)
    (let ((github-url "https://github.com/example-org/my-project")
          (gitlab-url "https://gitlab.example.com/internal/my-project.git"))
      (test-git:with-git-repo (repo :link github-url :dir dir)
        (test-git:make-commit repo "initial commit")
        (test-git:enable-server-features repo)
        ;; Set git remote to GitLab (simulates GitLab CI environment)
        (git::$ (git-command repo) "remote" "add" "origin" gitlab-url)
        ;; Confirm new flow will be used
        (is-true (new-flow-enabled-p self repo)
                 "New flow should be enabled for this test")
        ;; Mock fetch-remote-branch since we don't have a real remote
        (cl-mock:if-called 'fetch-remote-branch
                           (lambda (repo branch)
                             (declare (ignore repo branch))
                             (values)))
        ;; Run update-commit-graph (the high-level function)
        (update-commit-graph self repo "master")
        ;; The commit graph SHOULD be created for repo-link (GitHub URL)
        (let ((github-cg (find-commit-graph
                          company github-url)))
          (is-true github-cg
                   "Commit graph should be created for --repo-url (GitHub), not git remote"))
        ;; The commit graph should NOT be created for the git remote (GitLab URL)
        (let ((gitlab-cg (find-commit-graph
                          company gitlab-url)))
          (is-false gitlab-cg
                    "Commit graph should NOT be created for git remote (GitLab)"))))))

#+lispworks
(test want-remote-ref
  (is-true (want-remote-ref nil (list "master")
                            "abcd"
                            "refs/heads/master"))
  (is-false (want-remote-ref nil (list "master")
                             "abcd"
                             "refs/heads/boo"))
  (is-false
   (want-remote-ref
    (list
     (make-instance 'dto:git-ref
                    :sha "abcd"
                    :name "master"))
    (list "master")
    "abcd"
    "refs/heads/master"))

  (is-true
   (want-remote-ref
    (list
     (make-instance 'dto:git-ref
                    :sha "0011"
                    :name "master"))
    (list "master")
    "abcd"
    "refs/heads/master")))


(test stores-refs-after-updating
  #+lispworks
  (with-fixture state ()
    (test-git:with-git-repo (repo :dir dir)
      (test-git:make-commit repo "foo")
      (test-git:enable-server-features repo)
      (let ((upload-pack (local-upload-pack repo)))
        (update-from-pack
         self
         upload-pack
         (namestring dir)
         (list (git:current-branch repo))))
      (assert-that
       (get-commit-graph-refs
        self
        (namestring dir))
       (contains
        (has-typep 'dto:git-ref)))

      (is (equal (git:current-commit repo)
                 (gethash "refs/heads/master" (all-remote-refs self)))))))
