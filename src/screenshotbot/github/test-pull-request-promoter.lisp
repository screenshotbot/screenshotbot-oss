;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/test-pull-request-promoter
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:screenshotbot/github
        #:screenshotbot/model/test-object
        #:screenshotbot/model/company
        #:screenshotbot/compare
        #:screenshotbot/diff-report
        #:screenshotbot/github/access-checks
        #:screenshotbot/promote-api
        #:screenshotbot/model/channel
        #:screenshotbot/git-repo
        #:fiveam)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run
                #:pull-request-url)
  (:import-from #:screenshotbot/api/promote
                #:maybe-promote-run)
  (:import-from #:screenshotbot/github/pull-request-promoter
                #:send-task-args
                #:base-commit
                #:check-status
                #:check-title
                #:retrieve-run
                #:report)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:make-task-args
                #:make-check-result-from-diff-report)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:import-from #:screenshotbot/diff-report
                #:change)
  (:import-from #:screenshotbot/github/app-installation
                #:app-installed-p)
  (:import-from #:screenshotbot/github/settings
                #:verified-repo-p)
  (:import-from #:screenshotbot/github/pull-request-promoter
                #:plugin-installed?)
  (:import-from #:screenshotbot/github/pr-checks
                #:github-update-pull-request)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:screenshotbot/model/report
                #:acceptable-state)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:bknr.indices
                #:object-destroyed-p))
(in-package :screenshotbot/github/test-pull-request-promoter)

(util/fiveam:def-suite)

(defvar *base-run*)

(defclass pull-request-info ()
  ())

(defclass my-run-retriever ()
  ())

(defclass dummy-repo (github-repo)
  ())

(defclass dummy-channel (test-object)
  ((run :accessor channel-runs
        :initform nil)
   (name :accessor channel-name
         :initform "foo")
   (repo :transient t
         :initform (make-instance 'dummy-repo)
         :accessor channel-repo))
  (:metaclass persistent-class))

(defmethod repo-link ((repo dummy-repo))
  "https://github.com/foo/bar.git")

(defmethod retrieve-run ((retriever my-run-retriever)
                         channel base-commit)
  (is (equal "car" base-commit))
  *base-run*)


(def-fixture state (&key (run-retriever 'my-run-retriever))
  (with-installation ()
   (with-test-store ()
     (cl-mock:with-mocks ()
       (cl-mock:if-called 'verified-repo-p
                          (lambda (repo company)
                            t))
       (cl-mock:if-called 'app-installed-p
                          (lambda (repo)
                            t))
       (let ((company (make-instance 'company))
             (promoter (make-instance 'pull-request-promoter
                                      :app-id "dummy-app-id"
                                      :private-key "dummy-private-key"
                                      :pull-request-info
                                      (make-instance 'pull-request-info)
                                      :run-retriever
                                      (make-instance run-retriever))))
         (&body))))))

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

(test bitbucket-repo-doesnt-cause-promoter-to-crash
  (with-fixture state ()
    (let* ((repo (make-instance 'generic-git-repo
                                 :link "foo"))
           (channel (let ((channel (make-instance 'dummy-channel)))
                      (setf (channel-repo channel) repo)
                      channel))
           (run (make-instance 'recorder-run
                                :company company
                                :channel channel
                                :merge-base "car"
                                :commit-hash "foo")))
      (maybe-promote promoter run))))

(test plugin-installed?
  (with-fixture state ()
    (is-true (plugin-installed?
              promoter company "https://github.com/far/bar"))
    (cl-mock:if-called 'verified-repo-p
                        (lambda (repo company)
                          nil)
                        :at-start t)
    (is-false (plugin-installed?
               promoter company "https://github.com/far/bar"))))

(test plugin-installed?-should-return-false-if-app-not-installed
  (with-fixture state ()
    (cl-mock:if-called 'app-installed-p
                        (lambda (repo)
                          nil)
                        :at-start t)
    (is-false (plugin-installed?
               promoter company "https://github.com/far/bar"))))

(test run-with-pr-creates-a-report
  (with-fixture state ()
    (let ((*base-run* (make-instance
                       'recorder-run
                        :company company
                        :channel (make-instance 'dummy-channel)
                        :merge-base "dfdfdf"
                        :commit-hash "car"))
          (check))
      (cl-mock:if-called 'make-task-args
                         (lambda (promoter run repo %check)
                           (declare (ignore promoter run repo))
                           (setf check %check)))
      (let ((run (make-instance
                  'recorder-run
                  :company company
                  :channel (make-instance 'dummy-channel)
                  :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                  :merge-base "car"
                  :commit-hash "foo")))
       (maybe-promote promoter run)
       (is (equal "car" (base-commit promoter)))
       (is (eql :success (check-status check)))))))

(test without-a-base-run-we-get-an-error
  (with-fixture state ()
    (let ((*base-run* nil))
     (let ((run (make-instance
                 'recorder-run
                  :channel (make-instance 'dummy-channel)
                  :company company
                  :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                  :merge-base "car"
                  :commit-hash "foo"))
           (check))
       (cl-mock:if-called 'make-task-args
                          (lambda (promoter run repo %check)
                            (declare (ignore promoter run repo))
                            (setf check %check)))
       (maybe-promote promoter run)
       (is (equal "car" (base-commit promoter)))
       (is (eql :failure (check-status check)))
       (is (cl-ppcre:scan ".*rebasing*" (check-title check)))))))

(test check-result-for-diff-report
  (with-installation ()
   (with-test-store ()
     (let ((empty-run (make-instance 'recorder-run))
           (empty-report (make-instance 'diff-report :added nil
                                                     :deleted nil
                                                     :changes nil)))
       (let ((check (make-check-result-from-diff-report
                     (make-instance 'pull-request-promoter)
                     empty-report
                     empty-run nil)))
         (is (eql :success (check-status check)))
         (is (equal "No screenshots changed"
                    (check-title check))))))))

(test check-result-for-unempty-diff-report
  (with-installation ()
   (with-test-store ()
     (let ((diff-report (make-instance
                         'diff-report
                         :added nil
                         :deleted nil
                         :changes (list
                                   (make-instance
                                    'change
                                    :before (make-instance 'screenshot :name "foo")
                                    :after (make-instance 'screenshot :name "foo"))))))
       (let ((run (make-instance 'recorder-run
                                 :channel (make-instance 'dummy-channel))))
         (let ((check (make-check-result-from-diff-report
                       (make-instance 'pull-request-promoter)
                       diff-report
                       run run)))
           (is (eql :action_required(check-status check)))
           (is (cl-ppcre:scan "1 change.*" (check-title check)))))))))

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
                  :screenshots (list (make-instance 'screenshot :name "foobar"))
                  :merge-base "car"
                  :commit-hash "foo")))
       (maybe-promote promoter run)
       (is-true (report promoter))))))

(test maybe-send-tasks-happy-path
  (with-fixture state ()
    (cl-mock:with-mocks ()
      (let (calls)
        (cl-mock:if-called 'github-update-pull-request
                           (lambda (&rest args)
                             (push args calls)))
        (setf (send-task-args promoter) '(:dummy))
        (let ((run (make-instance
                    'recorder-run
                    :channel (make-instance 'dummy-channel)
                    :company company
                    :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                    :screenshots (list (make-instance 'screenshot :name "foobar"))
                    :merge-base "car"
                    :commit-hash "foo")))
          (maybe-send-tasks promoter run)
          (assert-that calls
                       (has-length 1)))))))

(test setf-acceptable-state-happy-path
  (with-fixture state ()
    (with-test-user (:logged-in-p t)
      (cl-mock:with-mocks ()
        (let (calls)
          (cl-mock:if-called 'github-update-pull-request
                             (lambda (&rest args)
                               (push args calls)))
          (let* ((run (make-instance
                       'recorder-run
                       :channel (make-instance 'dummy-channel)
                       :company company
                       :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                       :screenshots (list (make-instance 'screenshot :name "foobar"))
                       :merge-base "car"
                       :commit-hash "foo"))
                 (report (make-instance 'report :run run))
                 (acceptable (make-instance 'pr-acceptable
                                            :send-task-args nil
                                            :report report)))
            (setf (acceptable-state acceptable) :accepted)
            (assert-that calls
                         (has-length 1))))))))
