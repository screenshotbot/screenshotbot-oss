;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-promote
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:fiveam
        #:screenshotbot/api/promote
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/channel
        #:screenshotbot/model/company
        #:screenshotbot/model/user
        #:screenshotbot/git-repo
        #:screenshotbot/user-api
        #:screenshotbot/model/api-key)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:equal-to
                #:assert-that)
  (:import-from #:screenshotbot/git-repo
                #:generic-git-repo
                #:commit-graph)
  (:import-from #:screenshotbot/sdk/git
                #:git-repo)
  (:import-from #:screenshotbot/model/recorder-run
                #:gitlab-merge-request-iid
                #:assert-no-loops
                #:make-recorder-run
                #:not-fast-forward-promotion-warning
                #:recorder-run-warnings)
  (:import-from #:fiveam-matchers/lists
                #:contains))
(in-package :screenshotbot/api/test-promote)

(util/fiveam:def-suite)

(defun make-dag (commits)
  (let ((dag (make-instance 'dag:dag)))
    (loop for (key . parent) in commits
          do
             (dag:add-commit dag
                             (make-instance 'dag:commit
                                            :sha key
                                            :parents (list
                                                      parent))))
    dag))

(defparameter *tree* `(("a4" . "a2")
                       ("a5" . "a3")
                       ("a7" . "a6")
                       ("a3" . "a2")
                       ("a2" . "a1")))

(defclass dummy-repo (generic-git-repo)
  ((commits :initform *tree*)
   (dag :initform (make-dag *tree*)
        :reader repo-dag)))

(defclass test-commit-graph ()
  ((dag :initarg :dag
        :reader commit-graph-dag)))

(defmethod commit-graph ((repo dummy-repo))
  (make-instance 'test-commit-graph
                 :dag (repo-dag repo)))

(defclass test-channel (channel)
  ((%repo :initform (make-instance 'dummy-repo)
          :reader channel-repo))
  (:metaclass persistent-class))

(defmethod public-repo-p ((repo dummy-repo))
  nil)

(def-fixture state ()
  (with-test-store ()
   (with-fake-request ()
     (auth:with-sessions ()
       (with-test-user (:company company
                        :user user)
         (let* ((channel (make-instance 'test-channel :name "foo")))
           (with-transaction ()
             (push channel (company-channels company)))
           (flet ((make-run (&rest args)
                    (let ((run (apply #'make-recorder-run
                                      (append
                                       args
                                       (list
                                        :channel channel
                                        :github-repo "foo"
                                        :commit-hash nil
                                        :cleanp t
                                        :branch "master"
                                        :branch-hash "a1"
                                        :trunkp t
                                        :company company)))))
                      (with-transaction ()
                        (push run (company-runs company)))
                      run)))
            (let* ((run1 (make-run :commit-hash "a1"
                                   :branch "master"
                                   :branch-hash "a1"))
                   (run2 (make-run :commit-hash "a2"
                                   :branch "master"
                                   :branch-hash "a2")))
              (&body)))))))))

(defun %maybe-promote-run (run channel &key (wait-timeout 1))
  (maybe-promote-run run
                     :channel channel
                     :wait-timeout wait-timeout)
  (with-transaction ()
    (setf (promotion-complete-p run) channel)))

(test simple-promote-with-no-runs
  (with-fixture state ()
    (is-false (activep run1))
    (%maybe-promote-run run1 channel)
    (is-true (activep run1))
    (is-true (activep run1))

    ;; verify that run2 is untouched
    (is-false (activep run2))))

(test promote-second-run
  (with-fixture state ()
    (%maybe-promote-run run1 channel)
    (%maybe-promote-run run2 channel)

    (is-true (activep run2))
    (is-false (activep run1))
    (is (eql
         run1
         (recorder-previous-run run2)))))

(test dont-promote-a-run-with-a-merge-request
  (with-fixture state ()
    (setf (slot-value run2 'gitlab-merge-request-iid) 3443)
    (%maybe-promote-run run1 channel)
    (%maybe-promote-run run2 channel)

    (is-false (activep run2))
    (is-true (activep run1))))

(test dont-promote-a-run-with-a-pull-request
  (with-fixture state ()
    (setf (slot-value run2 'screenshotbot/model/recorder-run::pull-request) "foobar")
    (%maybe-promote-run run1 channel)
    (%maybe-promote-run run2 channel)

    (is-false (activep run2))
    (is-true (activep run1))))


(test dont-promote-run-with-identical-hash
  (with-fixture state ()
    (with-transaction ()
     (setf (recorder-run-commit run2) (recorder-run-commit run1)))
    (%maybe-promote-run run1 channel)
    (%maybe-promote-run run2 channel)

    (is-true (activep run1))
    (is-false (activep run2))))

(test dont-promote-older-hash
  (with-fixture state ()
    (%maybe-promote-run run2 channel :wait-timeout 0)
    (%maybe-promote-run run1 channel)

    (is-true (activep run2))
    (is-false (activep run1))))

(test unrelated-branch
  "If somebody `git push -f`-ed a new commit, then we'll get into this
situation. Can also happen on a developer branch."
  (with-fixture state ()
    (%maybe-promote-run run1 channel :wait-timeout 0)
    (let ((new-master-run (make-run :branch "master"
                                    :commit-hash "a6"
                                    :branch-hash "a6")))
      (%maybe-promote-run run2 channel :wait-timeout 0)
      (assert-that (activep run2)
                   (described-as "Sanity check"
                     (equal-to t)))

      (log:info "Starting next run")
      (%maybe-promote-run new-master-run channel :wait-timeout 0)

      (is-false (activep run2))
      (is-true (activep new-master-run))

      (assert-that (recorder-run-warnings new-master-run)
                   (contains
                    (has-typep 'not-fast-forward-promotion-warning))))))

(test unrelated-branch-does-not-create-a-loop
  "When promoting unrelated branches, you could sometimes create a loop in
the promotion history."
  (with-fixture state ()
    (%maybe-promote-run run1 channel :wait-timeout 0)
    (let ((new-master-run (make-run :branch "master"
                                    :commit-hash "a6"
                                    :branch-hash "a6")))
      (%maybe-promote-run run2 channel :wait-timeout 0)
      (assert-that (activep run2)
                   (described-as "Sanity check"
                     (equal-to t)))

      (log:info "Starting next run")
      (%maybe-promote-run new-master-run channel :wait-timeout 0)

      (is-false (activep run2))
      (is-true (activep new-master-run))

      (%maybe-promote-run run2 channel :wait-timeout 0)
      (assert-no-loops run2))))
