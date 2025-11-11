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
                #:%commit-graph-refs
                #:commit-graph-refs
                #:normalization-override
                #:merge-dag-into-commit-graph
                #:dag-v2
                #:%persisted-dag
                #:normalize-url
                #:needs-flush-p
                #:flush-dags)
  (:import-from #:cl-mock
                #:answer
                #:if-called)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:has-typep
                #:assert-that
                #:equal-to
                #:is-not)
  (:import-from #:fiveam-matchers/misc
                #:is-not-null)
  (:import-from #:util/store/simple-object-snapshot
                #:snapshot-slot-value)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :screenshotbot/model/test-commit-graph)

(util/fiveam:def-suite)

(def-fixture state (&key dir)
  (with-test-store (:dir dir)
    (let ((company (make-instance 'company))
          (other-company (make-instance 'company)))
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
      (merge-dag-into-commit-graph cg dag)
      (is-false (needs-flush-p cg)))))

(test simple-snapshot-and-restore
  (tmpdir:with-tmpdir (dir)
    (with-fixture state (:dir dir)
      (let ((cg (find-or-create-commit-graph company "Foo")))
        (merge-dag-into-commit-graph cg dag)
        (util:safe-snapshot ".." t)))
    (with-test-store (:dir dir))))

(test flush-dags
  (with-fixture state ()
    (let ((cg (find-or-create-commit-graph company "Foo")))
      (merge-dag-into-commit-graph cg dag)
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
             (normalize-url "git@github.com/tdrhq/fast-example/")))
  (is (equal "https://github.com/tdrhq/fast-example"
             (normalize-url "git@github.com:tdrhq/fast-example/")))
  (is (equal "https://github.com/tdrhq/fast-example"
             (normalize-url "ssh://git@github.com:tdrhq/fast-example/"))))

(test normalize-url-removes-auth-info
  (is (equal
       "https://gitlab.com/abc/def/g/hij/klm"
       (normalize-url "https://gitlab-ci-token:blahblah@gitlab.com/abc/def/g/hij/klm"))))

(test normalization-overrides
  (with-fixture state ()
    (make-instance 'normalization-override
                   :from "foobar"
                   :to "bar")
    (is (equal "bar" (normalize-url "foobar")))
    (is (equal "https://github.com/tdrhq/fast-example"
               (normalize-url "git@github.com/tdrhq/fast-example/")))))

(test find-or-create-normalized-commit-graph
  (with-fixture state ()
    (is (eql
         (find-or-create-commit-graph company "https://github.com/tdrhq/fast-example")
         (find-or-create-commit-graph company "https://github.com/tdrhq/fast-example.git")))))

(test if-normalization-algorithm-changes-we-still-use-url
  (with-fixture state ()
    (let ((original (find-or-create-commit-graph company "https://github.com/tdrhq/fast-example")))
      (cl-mock:with-mocks ()
        (answer (normalize-url "https://github.com/tdrhq/fast-example") "foo")
        (is (eql original
                 (find-or-create-commit-graph company "https://github.com/tdrhq/fast-example")))))))

(test we-only-return-a-commit-graph-for-the-right-company
  (with-fixture state ()
    (let ((first-version (find-or-create-commit-graph other-company "https://github.com/tdrhq/fast-example.git")))
      (assert-that
       (find-or-create-commit-graph company "https://github.com/tdrhq/fast-example")
       (is-not
        (equal-to
         first-version))
       (is-not-null)))))

(test pulls-the-graph-for-the-right-company
  (with-fixture state ()
    (let ((other-cg (find-or-create-commit-graph other-company "https://github.com/tdrhq/fast-example.git"))
          (cg (find-or-create-commit-graph company "https://github.com/tdrhq/fast-example.git")))
      (assert-that
       (find-or-create-commit-graph company "https://github.com/tdrhq/fast-example")
       (equal-to cg))
      (assert-that
       (find-or-create-commit-graph other-company "https://github.com/tdrhq/fast-example")
       (equal-to other-cg)))))

(test snapshot-slot-value
  (with-fixture state ()
    (let ((cg (find-or-create-commit-graph company "foo")))
      (setf (%persisted-dag cg) nil #| probably already nil though |#)
      (is (eql nil (snapshot-slot-value cg 'dag-v2)))
      (setf (%persisted-dag cg) dag)
      (assert-that (snapshot-slot-value cg 'dag-v2)
                   (has-typep 'dag:dag)
                   (is-not (is-equal-to dag))))))

(test merging-into-graph
  (with-fixture state ()
    (let ((cg (find-or-create-commit-graph company "foo")))
      (merge-dag-into-commit-graph cg dag)
      (is-true (%persisted-dag cg))
      (is-true (dag:get-commit (%persisted-dag cg) "aa"))
      (is-false (dag:get-commit (%persisted-dag cg) "bb"))
      (merge-dag-into-commit-graph cg dag)
      (let ((dag (make-instance 'dag:dag)))
        (dag:add-commit dag (make-instance 'dag:commit
                                           :sha "bb"))
        (merge-dag-into-commit-graph cg dag)
        (is-true (dag:get-commit (%persisted-dag cg) "aa"))
        (is-true (dag:get-commit (%persisted-dag cg) "bb"))))))

(test merging-a-non-shallow-commit-into-a-shallow-commit
  "There are probably old versions of the CLIs still sending shallow
commits every now and then and we need to handle it."
  (with-fixture state ()
    (let ((cg (find-or-create-commit-graph company "foo")))
      (merge-dag-into-commit-graph cg dag)
      (is-true (%persisted-dag cg))
      (is-true (dag:get-commit (%persisted-dag cg) "aa"))
      (assert-that (dag:parents (dag:get-commit (%persisted-dag cg) "aa"))
                   (has-length 0))

      (let ((dag (make-instance 'dag:dag)))
        (dag:add-commit dag (make-instance 'dag:commit
                                           :sha "aa"
                                           :parents (list "cc")))
        (merge-dag-into-commit-graph cg dag)
        (is-true (dag:get-commit (%persisted-dag cg) "aa"))
        (assert-that (dag:parents (dag:get-commit (%persisted-dag cg) "aa"))
                     (has-length 1))
        (is-false (dag:get-commit (%persisted-dag cg) "cc"))))))


(test commit-graph-refs-when-refs-is-nil
  (with-fixture state ()
    (let ((cg (find-or-create-commit-graph company "foo")))
      (is (fset:equal?
           (fset:empty-map)
           (commit-graph-refs cg))))))

(test commit-graph-refs-when-refs-from-old-model
  (with-fixture state ()
    (let ((cg (find-or-create-commit-graph company "foo")))
      (setf (%commit-graph-refs cg)
            `(("foo" . "bar")
              ("car" . "dar")))
      (is (fset:equal?
           (fset:with
            (fset:with
             (fset:empty-map)
             "foo" "bar")
            "car" "dar")
           (commit-graph-refs cg))))))
