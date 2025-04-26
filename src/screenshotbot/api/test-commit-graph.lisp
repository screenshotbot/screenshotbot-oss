;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-commit-graph
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/api/commit-graph
                #:get-refs
                #:update-refs
                #:update-commit-graph
                #:%update-commit-graph-v2)
  (:import-from #:screenshotbot/git-repo
                #:find-or-create-commit-graph)
  (:import-from #:screenshotbot/model/commit-graph
                #:commit-graph-refs)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains-in-any-order
                #:contains)
  (:local-nicknames (#:a #:alexandria)
                    (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/test-commit-graph)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:logged-in-p t)
     (&body))))

(test update-commit-graph-happy-path ()
  (with-fixture state ()
   (let ((dag (make-instance 'dag:dag)))
     (dag:add-commit dag (make-instance 'dag:commit
                                        :sha "abcd"
                                        :author "zoidberg") )
     (finishes
       (%update-commit-graph-v2 "https://github.com/foo/bar"
                                (with-output-to-string (out)
                                  (dag:write-to-stream dag out)))))))


(test cannot-update-commit-graph-with-empty-repo
  (with-fixture state ()
    (finishes
     (update-commit-graph
      :repo-url ""
      :graph-json "{}"
      :format "json"))
    (signals error
      (update-commit-graph
      :repo-url nil
      :graph-json "{}"
      :format "json"))))

(defvar *repo* "https://github.com/tdrhq/fast-example.git")

(test update-refs
  (with-fixture state ()
   (finishes
     (update-refs :repo-url *repo*
                  :refs "[{\"name\":\"master\",\"sha\":\"abcd\"}]"))
    (let ((cg (find-or-create-commit-graph (auth:current-company) *repo*)))
      (assert-that
       (commit-graph-refs cg)
       (contains
        '("master" . "abcd")))
      (update-refs :repo-url *repo*
                   :refs "[{\"name\":\"master\",\"sha\":\"0011\"},{\"name\":\"feature\",\"sha\":\"abcd\"}]")
      (assert-that
       (commit-graph-refs cg)
       (contains-in-any-order
        '("master" . "0011")
        '("feature" . "abcd"))))))

(test get-refs
  (with-fixture state ()
    (finishes
      (update-refs :repo-url *repo*
                   :refs "[{\"name\":\"master\",\"sha\":\"abcd\"}]"))
    (assert-that (get-refs :repo-url *repo*)
                 (contains
                  (has-typep 'dto:git-ref)))))
