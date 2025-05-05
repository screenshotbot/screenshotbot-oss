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
  (:local-nicknames (#:dto #:screenshotbot/api/model)
                    (#:test-git #:screenshotbot/sdk/test-git)
                    (#:git #:screenshotbot/sdk/git)))
(in-package :screenshotbot/sdk/test-commit-graph)


(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
   (with-sdk-integration (api-context :company company)
     (&body))))

(defvar *repo* "https://github.com/tdrhq/fast-example.git")

(test happy-path-without-refs
  (with-fixture state ()
    (is (eql nil (get-commit-graph-refs
                  api-context
                  *repo*)))))

(test case-with-repo-though
  (with-fixture state ()
    (make-instance 'commit-graph
                   :company company
                   :url *repo*
                   :refs `(("master" . "abcd")))
    (assert-that
     (get-commit-graph-refs
      api-context
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
       (update-commit-graph api-context
                            repo
                            "main")))))


(test new-flow-enabled-p-happy-path
  (with-fixture state ()
    (answer (uiop:getenv "SCREENSHOTBOT_ENABLE_UPLOAD_PACK") "true")
    (test-git:with-git-repo (repo :dir dir)
      (is-false (new-flow-enabled-p repo))
      (git::$ (git-command repo) "remote" "add" "origin" "git@github.com:tdrhq/fast-example.git")
      (#+lispworks is-true #-lispworks is-false (new-flow-enabled-p repo)))))

#+lispworks
(test update-from-pack
  (with-fixture state ()
    (test-git:with-git-repo (repo :dir dir)
      (test-git:make-commit repo "foo")
      (let ((upload-pack (local-upload-pack repo)))
        (update-from-pack
         api-context
         upload-pack
         "git@github.com:tdrhq/fast-example.git"
         (list (git:current-branch repo)))))))

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
