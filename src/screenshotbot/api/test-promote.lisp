;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/api/test-promote
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
  (:import-from #:screenshotbot/api/promote
                #:*promotion-log-stream*)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:fiveam-matchers/core
                #:equal-to
                #:assert-that))

(util/fiveam:def-suite)

(defclass dummy-repo ()
  ((commits :initform `(("master" . "car2")
                        ("new-master" . "bar")
                        ("force-master" . "qux")
                        ("bar" . "car2")
                        ("car2" . "car")))))

(defclass test-channel (channel)
  ()
  (:metaclass persistent-class))

(defmethod channel-repo ((c test-channel))
  (make-instance 'dummy-repo))

(defmethod public-repo-p ((repo dummy-repo))
  nil)

(defmethod get-parent-commit ((repo dummy-repo) commit)
  (with-slots (commits) repo
    (assoc-value commits commit :test #'equal)))

(defmethod merge-base ((repo dummy-repo) commit1 commit2)
  (flet ((one-way (commit1 commit2)
           (let (seen)
             (labels ((dfs (commit)
                        (when commit
                          (push commit seen)
                          (dfs (get-parent-commit repo commit)))))
               (dfs commit1))
             (labels ((dfs (commit)
                        (when commit
                          (cond
                            ((str:s-member seen commit)
                             commit)
                            (t
                             (dfs  (get-parent-commit repo commit)))))))
               (dfs commit2)))))
    (or (one-way commit1 commit2)
        (one-way commit2 commit1))))

(defmethod repo-ancestor-p ((repo dummy-repo) commit1 commit2)
  (equal commit1 (merge-base repo commit1 commit2)))

(defmethod repo-left-ancestor-p ((repo dummy-repo) commit1 commit2)
  (equal commit1 (merge-base repo commit1 commit2)))




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
                    (let ((run (apply #'make-instance
                                  'recorder-run
                                  (append
                                   args
                                   (list
                                    :channel channel
                                    :github-repo "foo"
                                    :commit-hash nil
                                    :cleanp t
                                    :branch "master"
                                    :branch-hash "car"
                                    :trunkp t
                                    :company company)))))
                      (with-transaction ()
                        (push run (company-runs company)))
                      run)))
            (let* ((run1 (make-run :commit-hash "car"
                                   :branch "master"
                                   :branch-hash "car"))
                   (run2 (make-run :commit-hash "car2"
                                   :branch "master"
                                   :branch-hash "car2")))
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
                                    :commit-hash "qux"
                                    :branch-hash "qux")))
      (%maybe-promote-run run2 channel :wait-timeout 0)
      (assert-that (activep run2)
                   (described-as "Sanity check"
                     (equal-to t)))

      (log:info "Starting next run")
      (%maybe-promote-run new-master-run channel :wait-timeout 0)

      (is-true (activep run2))
      (is-false (activep new-master-run)))))
