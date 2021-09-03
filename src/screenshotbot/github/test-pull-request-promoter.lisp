;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/github/test-pull-request-promoter
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:../github
        #:../model/test-object
        #:../model/company
        #:../compare
        #:./access-checks
        #:../model/recorder-run
        #:../promote-api
        #:../model/channel
        #:../git-repo
        #:fiveam)
  (:import-from #:../api/promote
                #:maybe-promote-run)
  (:import-from #:./pull-request-promoter
                #:base-commit
                #:promoter-result
                #:check-status
                #:check-title
                #:retrieve-run
                #:make-check-result-from-diff-report
                #:report)
  (:import-from #:screenshotbot/model/company
                #:installation-id))

(util/fiveam:def-suite)

(defvar *base-run*)

(defclass pull-request-info ()
  ())

(defclass my-run-retriever ()
  ())

(defclass dummy-channel (test-object)
  ((run :accessor channel-runs
        :initform nil)
   (name :accessor channel-name
         :initform "foo"))
  (:metaclass persistent-class))

(defclass dummy-repo (github-repo)
  ())

(defmethod repo-link ((repo dummy-repo))
  "https://github.com/foo/bar.git")

(defmethod channel-repo ((channel dummy-channel))
  (make-instance 'dummy-repo))

(defmethod retrieve-run ((retriever my-run-retriever)
                         channel base-commit)
  (is (equal "car" base-commit))
  *base-run*)


(def-fixture state (&key (run-retriever 'my-run-retriever))
  (cl-mock:with-mocks ()
    (cl-mock:if-called 'installation-id
                        (lambda (&rest args)
                          (declare (ignore args))
                          "mocked-installation-id"))
    (let ((company (make-instance 'company))
          (promoter (make-instance 'pull-request-promoter
                                    :app-id "dummy-app-id"
                                    :private-key "dummy-private-key"
                                    :pull-request-info
                                    (make-instance 'pull-request-info)
                                    :run-retriever
                                    (make-instance run-retriever))))
      (&body))))

(test run-without-pr-does-not-create-report
  (with-fixture state ()
    (let* ((*base-run* nil)
           (run (make-instance 'recorder-run
                                   :company company
                                   :channel (make-instance 'dummy-channel)
                                   :merge-base "car"
                                   :commit-hash "foo")))
      (maybe-promote promoter run)
      (is-false (report promoter))
      (is (equal "car"(base-commit promoter))))))

(test run-with-pr-creates-a-report
  (with-fixture state ()
    (let ((*base-run* (make-instance
                       'recorder-run
                        :company company
                        :channel (make-instance 'dummy-channel)
                        :merge-base "dfdfdf"
                        :commit-hash "car")))
     (let ((run (make-instance
                 'recorder-run
                  :company company
                  :channel (make-instance 'dummy-channel)
                  :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                  :merge-base "car"
                  :commit-hash "foo")))
       (maybe-promote promoter run)
       (is (equal "car" (base-commit promoter)))
       (let ((check (promoter-result promoter)))
         (is (eql :success (check-status check))))))))

(test without-a-base-run-we-get-an-error
  (with-fixture state ()
    (let ((*base-run* nil))
     (let ((run (make-instance
                 'recorder-run
                  :channel (make-instance 'dummy-channel)
                  :company company
                  :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                  :merge-base "car"
                  :commit-hash "foo")))
       (maybe-promote promoter run)
       (is (equal "car" (base-commit promoter)))
       (let ((check (promoter-result promoter)))
         (is (eql :failure (check-status check)))
         (is (cl-ppcre:scan ".*rebasing*" (check-title check))))))))

(test check-result-for-diff-report
  (let ((empty-report (make-instance 'diff-report :added nil
                                                  :deleted nil
                                                  :changes nil)))
    (let ((check (make-check-result-from-diff-report
                  (make-instance 'pull-request-promoter)
                  empty-report
                  nil nil)))
      (is (eql :success (check-status check)))
      (is (equal "No screenshots changed"
                 (check-title check))))))

(test check-result-for-unempty-diff-report
  (let ((diff-report (make-instance 'diff-report :added nil
                                                 :deleted nil
                                                 :changes (list :dummy))))
    (let ((run (make-instance 'recorder-run
                              :channel (make-instance 'dummy-channel))))
      (let ((check (make-check-result-from-diff-report
                    (make-instance 'pull-request-promoter)
                    diff-report
                    run run)))
       (is (eql :action_required(check-status check)))
       (is (cl-ppcre:scan "1 change.*" (check-title check)))))))

(test report-has-acceptable
  (with-fixture state ()
    (let ((*base-run* (make-instance
                       'recorder-run
                        :company company
                        :channel (make-instance 'dummy-channel)
                        :commit-hash "car")))
     (let ((run (make-instance
                 'recorder-run
                  :channel (make-instance 'dummy-channel)
                  :company company
                  :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                  :screenshots (list 1) ;; todo: use actual screenshot here
                  :merge-base "car"
                  :commit-hash "foo")))
       (maybe-promote promoter run)
       (is-true (report promoter))))))
