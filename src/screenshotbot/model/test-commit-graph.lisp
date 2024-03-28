;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-commit-graph
  (:use #:cl
        #:fiveam
        #:screenshotbot/model/commit-graph
        #:screenshotbot/model/company)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/commit-graph
                #:normalize-url
                #:needs-flush-p
                #:flush-dags))
(in-package :screenshotbot/model/test-commit-graph)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company)))
      (let ((dag (make-instance 'dag:dag)))
        (dag:add-commit dag (make-instance 'dag:commit
                                       :sha "aa") )
       (&body)))))

(test simple-find-or-create
  (with-fixture state ()
    (is (eql
         (find-or-create-commit-graph company "foo")
         (find-or-create-commit-graph company "foo")))))

(test set-and-get-dag
  (with-fixture state ()
    (let ((cg (find-or-create-commit-graph company "Foo")))
      (setf (commit-graph-dag cg) dag)
      (is (eql dag (commit-graph-dag cg)))
      (is-true (needs-flush-p cg)))))

(test flush-dags
  (with-fixture state ()
    (let ((cg (find-or-create-commit-graph company "Foo")))
      (setf (commit-graph-dag cg) dag)
      (finishes
        (flush-dags))
      (is-false (needs-flush-p cg)))))

(test normalize-url
  (is (equal "https://github.com/tdrhq/fast-example"
             (normalize-url "https://github.com/tdrhq/fast-example.git")))
  (is (equal "https://github.com/tdrhq/fast-example"
             (normalize-url "https://github.com/tdrhq/fast-example.git/")))
  (is (equal "https://github.com/tdrhq/fast-example"
             (normalize-url "https://github.com/tdrhq/fast-example/")))
  (is (equal "https://github.com/tdrhq/fast-example"
             (normalize-url "https://github.com/tdrhq/Fast-Example/")))
  (is (equal "https://github.com/tdrhq/fast-example"
             (normalize-url "git@github.com/tdrhq/fast-example/"))))
