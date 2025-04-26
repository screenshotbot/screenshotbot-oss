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
                #:get-commit-graph-refs)
  (:import-from #:screenshotbot/git-repo
                #:commit-graph)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/test-commit-graph)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-sdk-integration (api-context :company company)
    (&body)))

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

