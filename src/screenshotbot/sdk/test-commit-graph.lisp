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
                #:commit-graph-updater
                #+lispworks
                #:want-remote-ref
                #+lispworks
                #:update-from-pack
                #:new-flow-enabled-p
                #:update-commit-graph
                #:get-commit-graph-refs)
  (:import-from #:screenshotbot/git-repo
                #:commit-graph)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:screenshotbot/sdk/git
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

(test new-flow-disabled-with-override-commit-hash
  (with-fixture state ()
    (gk:enable :cli-shallow-clones)
    (test-git:with-git-repo (repo :dir dir)
      (test-git:make-commit repo "foo")
      (git::$ (git-command repo) "remote" "add" "origin" "git@github.com:tdrhq/fast-example.git")
      (is-false
       (new-flow-enabled-p self repo
                           :override-commit-hash "abcd"))
     (#+lispworks is-true #-lispworks is-false
       (new-flow-enabled-p self repo
                           :override-commit-hash (git:current-commit repo))))))

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
          (list (git:current-branch repo))))))))

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
