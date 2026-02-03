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
                #:find-missing-commits
                #:remove-duplicate-commits
                #:get-local-commits
                #:update-commit-graph-new-style
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
                #:git-repo
                #:repo-link
                #:get-remote-url
                #:git-command
                #:fetch-remote-branch)
  (:import-from #:cl-mock
                #:answer
                #:if-called)
  #+lispworks
  (:import-from #:screenshotbot/sdk/git-pack
                #:make-remote-upload-pack
                #:local-upload-pack)
  (:import-from #:screenshotbot/api/core
                #:*wrap-internal-errors*)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context-prepared-p
                #:api-feature-enabled-p
                #:api-features)
  (:import-from #:fiveam-matchers/misc
                #:is-not-null)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
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
           :local-commits (list (list "aaaa" ;; <-- fake local commit
                                       (git:current-commit repo)))))
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

(test clone-and-update-commit-graph
  #+lispworks
  (with-fixture state ()
    (gk:enable :cli-shallow-clones)
    (test-git:with-git-repo (repo1 :dir dir1)
      ;; Create initial repo with one commit
      (test-git:make-commit repo1 "initial commit")
      (test-git:enable-server-features repo1)

      ;; Clone the repo to a new location
      (tmpdir:with-tmpdir (dir)
        (uiop:run-program
         (list "git" "clone" (namestring dir1)
               (namestring (path:catdir dir "repo2/"))))
        (let ((repo2 (make-instance 'git-repo :dir (path:catdir dir "repo2/") :link (namestring dir1))))
          ;; Add a new commit to the cloned repo
          (test-git:make-commit repo2 "second commit")
          (test-git:enable-server-features repo2)

          (assert-that (get-local-commits repo2)
                       (has-length 2))
          ;; Set git remote to enable new flow
          (finishes
            (update-commit-graph-new-style self repo2 (git:current-branch repo2)))

          ;; Verify the commit graph contains both commits
          (let* ((commit-graph (find-commit-graph company (namestring dir1)))
                 (dag (commit-graph-dag commit-graph)))
            (assert-that (dag:get-commit dag (git:current-commit repo1))
                         (is-not-null))
            (assert-that (dag:get-commit dag (git:current-commit repo2))
                         (is-not-null))))))))

(test shallow-clone-and-update-commit-graph
  #+lispworks
  (with-fixture state ()
    (gk:enable :cli-shallow-clones)
    (test-git:with-git-repo (repo1 :dir dir1)
      ;; Create 10 commits in the original repo
      (loop for i from 1 to 2
            do (test-git:make-commit repo1 (format nil "commit ~a" i)))
      (test-git:enable-server-features repo1)

      ;; Shallow clone with depth 1
      ;; Use file:// URL to prevent local clone optimizations (hardlinks/alternates)
      (tmpdir:with-tmpdir (dir)
        (uiop:run-program
         (list "git" "clone" "--no-local" "--depth" "1" (namestring dir1)
               (namestring (path:catdir dir "repo2/"))))
        (let ((repo2 (make-instance 'git-repo :dir (path:catdir dir "repo2/") :link (namestring dir1))))
          ;; Add a new commit to the shallow cloned repo
          (test-git:make-commit repo2 "new local commit")
          ;; Don't enable server features on shallow clone - it can affect shallow boundaries

          ;; Shallow clone should only have 2 commits visible locally:
          ;; the grafted commit from origin + our new commit
          (assert-that (get-local-commits repo2)
                       (has-length 2))

          ;; Update the commit graph
          (finishes
            (update-commit-graph-new-style self repo2 (git:current-branch repo2)))

          ;; Verify the commit graph contains both the latest from repo1 and our new commit
          (let* ((commit-graph (find-commit-graph company (namestring dir1)))
                 (dag (commit-graph-dag commit-graph)))
            (assert-that (dag:get-commit dag (git:current-commit repo1))
                         (is-not-null))
            (assert-that (dag:get-commit dag (git:current-commit repo2))
                         (is-not-null))))))))

;;; --- Unit tests for find-missing-commits ---

(test find-missing-commits-chain-with-missing-parent
  "A -> B -> C where C is not in the commit list"
  (let ((commits (list (list "aaa" "bbb")
                       (list "bbb" "ccc"))))
    (is (equal (list "ccc")
               (find-missing-commits commits)))))

(test find-missing-commits-all-parents-present
  "All parents are present in the commit list"
  (let ((commits (list (list "aaa" "bbb")
                       (list "bbb"))))
    (is (equal nil
               (find-missing-commits commits)))))

(test find-missing-commits-empty-input
  (is (equal nil (find-missing-commits nil))))

(test find-missing-commits-root-commits
  "Root commits have no parents -- should not produce nil in the missing list"
  (let ((commits (list (list "aaa")
                       (list "bbb" "aaa"))))
    (is (equal nil (find-missing-commits commits)))))

;;; --- Unit tests for remove-duplicate-commits ---

(test remove-duplicate-commits-keeps-first
  (let ((commits (list (list "aaa" "bbb")
                       (list "ccc" "ddd")
                       (list "aaa" "eee"))))
    (is (equal (list (list "aaa" "bbb")
                     (list "ccc" "ddd"))
               (remove-duplicate-commits commits)))))

(test remove-duplicate-commits-no-dupes
  (let ((commits (list (list "aaa" "bbb")
                       (list "ccc" "ddd"))))
    (is (equal commits
               (remove-duplicate-commits commits)))))

(test remove-duplicate-commits-empty
  (is (equal nil (remove-duplicate-commits nil))))

;;; --- update-from-pack with nil local-commits ---

#+lispworks
(test update-from-pack-with-nil-local-commits
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
          (list (git:current-branch repo))
          :local-commits nil))

        (let* ((commit-graph (first (bknr.datastore:class-instances 'commit-graph)))
               (dag (commit-graph-dag commit-graph)))
          (assert-that (dag:get-commit dag (git:current-commit repo))
                       (is-not-null)))))))

;;; --- Second call to update-commit-graph-new-style after new local commit ---

#+lispworks
(test update-commit-graph-new-style-twice
  (with-fixture state ()
    (gk:enable :cli-shallow-clones)
    (test-git:with-git-repo (repo1 :dir dir1)
      (test-git:make-commit repo1 "initial commit")
      (test-git:enable-server-features repo1)

      (tmpdir:with-tmpdir (dir)
        (uiop:run-program
         (list "git" "clone" (namestring dir1)
               (namestring (path:catdir dir "repo2/"))))
        (let ((repo2 (make-instance 'git-repo :dir (path:catdir dir "repo2/") :link (namestring dir1))))
          (test-git:make-commit repo2 "second commit")
          (test-git:enable-server-features repo2)

          ;; First upload
          (finishes
            (update-commit-graph-new-style self repo2 (git:current-branch repo2)))

          (let ((commit-after-first (git:current-commit repo2)))
            ;; Make another local commit
            (test-git:make-commit repo2 "third commit")

            ;; Second upload should not fail
            (finishes
              (update-commit-graph-new-style self repo2 (git:current-branch repo2)))

            ;; All three commits should be in the graph
            (let* ((commit-graph (find-commit-graph company (namestring dir1)))
                   (dag (commit-graph-dag commit-graph)))
              (assert-that (dag:get-commit dag (git:current-commit repo1))
                           (is-not-null))
              (assert-that (dag:get-commit dag commit-after-first)
                           (is-not-null))
              (assert-that (dag:get-commit dag (git:current-commit repo2))
                           (is-not-null)))))))))

;;; --- Merge commits with multiple parents ---

#+lispworks
(test clone-and-update-commit-graph-with-merge
  (with-fixture state ()
    (gk:enable :cli-shallow-clones)
    (test-git:with-git-repo (repo1 :dir dir1)
      (test-git:make-commit repo1 "initial commit")
      (test-git:enable-server-features repo1)

      (tmpdir:with-tmpdir (dir)
        (uiop:run-program
         (list "git" "clone" (namestring dir1)
               (namestring (path:catdir dir "repo2/"))))
        (let ((repo2 (make-instance 'git-repo :dir (path:catdir dir "repo2/") :link (namestring dir1))))
          (test-git:enable-server-features repo2)

          ;; Create a branch with a commit (use a different file to avoid merge conflict)
          (git::$ (git-command repo2) "checkout" "-b" "feature")
          (test-git:make-commit repo2 "feature content" :file "feature.txt")
          (let ((feature-commit (git:current-commit repo2)))

            ;; Go back to master and make a different commit (writes to file.txt)
            (git::$ (git-command repo2) "checkout" "master")
            (test-git:make-commit repo2 "master commit")

            ;; Merge feature into master (creates a merge commit with 2 parents)
            (git::$ (git-command repo2) "merge" "--no-edit" "feature")
            (let ((merge-commit (git:current-commit repo2)))

              ;; Verify local commits include the merge commit with multiple parents
              (let* ((local-commits (get-local-commits repo2))
                     (merge-entry (find merge-commit local-commits
                                        :key #'car :test #'equal)))
                (is (>= (length (cdr merge-entry)) 2)
                    "Merge commit should have at least 2 parents"))

              ;; find-missing-commits should handle multiple parents
              (finishes
                (update-commit-graph-new-style self repo2 (git:current-branch repo2)))

              ;; Verify the graph has the merge commit and the feature commit
              (let* ((commit-graph (find-commit-graph company (namestring dir1)))
                     (dag (commit-graph-dag commit-graph)))
                (assert-that (dag:get-commit dag merge-commit)
                             (is-not-null))
                (assert-that (dag:get-commit dag feature-commit)
                             (is-not-null))))))))))

;;; --- find-missing-commits with merge commits ---

(test find-missing-commits-with-merge
  "Merge commit has two parents, both should be checked"
  (let ((commits (list (list "merge" "aaa" "bbb")
                       (list "aaa"))))
    (is (equal (list "bbb")
               (find-missing-commits commits)))))

