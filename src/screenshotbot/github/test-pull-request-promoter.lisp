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
                #:make-recorder-run
                #:recorder-run
                #:pull-request-url)
  (:import-from #:screenshotbot/api/promote
                #:maybe-promote-run)
  (:import-from #:screenshotbot/github/pull-request-promoter
                #:make-github-args
                #:send-task-args
                #:check-status
                #:check-title
                #:retrieve-run
                #:report)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:make-check
                #:check
                #:push-remote-check
                #:check-status
                #:pr-merge-base
                #:make-check-result-from-diff-report)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot)
  (:import-from #:screenshotbot/diff-report
                #:make-diff-report
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
                #:object-destroyed-p)
  (:import-from #:cl-mock
                #:answer)
  (:import-from #:screenshotbot/github/plugin
                #:github-plugin)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:alexandria
                #:plist-alist
                #:alist-plist
                #:assoc-value)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot))
(in-package :screenshotbot/github/test-pull-request-promoter)

(util/fiveam:def-suite)

(defvar *base-run*)

(defclass pull-request-info ()
  ())

(defclass my-run-retriever ()
  ())

(defclass dummy-repo (github-repo)
  ())

(defclass dummy-channel (channel)
  ((name :accessor channel-name
         :initform "foo")
   (repo :transient t
         :initform (make-instance 'dummy-repo)
         :accessor channel-repo))
  (:metaclass persistent-class))

(defmethod repo-link ((repo dummy-repo))
  "https://github.com/foo/bar.git")

(defmethod retrieve-run ((retriever my-run-retriever)
                         channel base-commit
                         logger)
  (is (equal "car" base-commit))
  (lparallel:delay
   *base-run*))


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
        (cl-mock:answer (github-plugin)
          (make-instance 'github-plugin
                         :app-id "dummy-app-id"
                         :private-key "dummy-private-key"))
        (cl-mock:if-called 'github-update-pull-request
                           (lambda (&rest args)))
        (let ((auto-restart:*global-enable-auto-retries-p* nil)
              (company (make-instance 'company))
              (promoter (make-instance 'pull-request-promoter
                                       :pull-request-info
                                       (make-instance 'pull-request-info)
                                       :run-retriever
                                       (make-instance run-retriever))))
          (flet ((the-only-report ()
                   (let ((reports (class-instances 'report)))
                     (trivia:match reports
                       (nil
                        nil)
                       ((list report)
                        report)
                       (t
                        (error "Expected to have only one report but got: ~a" reports))))))
            (&body)))))))

(test run-without-pr-does-not-create-report
  (with-fixture state ()
    (let* ((*base-run* nil)
           (run (make-recorder-run
                 :company company
                 :github-repo "https://github.com/tdrhq/fast-example"
                 :channel (make-instance 'dummy-channel)
                 :merge-base "car"
                 :commit-hash "foo")))
      (maybe-promote promoter run)
      (is-false (the-only-report))
      (is (equal "car" (pr-merge-base promoter run))))))

(test bitbucket-repo-doesnt-cause-promoter-to-crash
  (with-fixture state ()
    (let* ((repo (make-instance 'generic-git-repo
                                 :link "foo"))
           (channel (let ((channel (make-instance 'dummy-channel)))
                      (setf (channel-repo channel) repo)
                      channel))
           (run (make-recorder-run
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
    (let ((*base-run* (make-recorder-run
                        :company company
                        :channel (make-instance 'dummy-channel)
                        :merge-base "dfdfdf"
                        :commit-hash "car"))
          (check))
      (cl-mock:if-called 'push-remote-check
                         (lambda (promoter run %check)
                           (declare (ignore promoter run))
                           (setf check %check)))
      (let ((run (make-recorder-run
                  :company company
                  :channel (make-instance 'dummy-channel)
                  :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                  :merge-base "car"
                  :commit-hash "foo")))
        (maybe-promote promoter run)
        (is-true check)
        (is (equal "car" (pr-merge-base promoter run)))
        (is (eql :success (check-status check)))))))

(test run-on-merge-queue-is-ignored
  (with-fixture state ()
    (let ((*base-run* (make-recorder-run
                        :company company
                        :channel (make-instance 'dummy-channel)
                        :merge-base "dfdfdf"
                        :commit-hash "car"))
          (check))
      (cl-mock:if-called 'push-remote-check
                         (lambda (promoter run %check)
                           (declare (ignore promoter run))
                           (setf check %check)))
      (let ((run (make-recorder-run
                  :company company
                  :channel (make-instance 'dummy-channel)
                  :work-branch "gh-readonly-queue/main/pr-45902-592fa2c43487bf"
                  :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                  :merge-base "car"
                  :commit-hash "foo")))
        (maybe-promote promoter run)
        (is-true check)
        (is (eql :success (check-status check)))
        (is (equal "Nothing to review" (check-title check)))))))'

(test run-on-main-is-ignored----sort-of
  (with-fixture state ()
    (let ((*base-run* (make-recorder-run
                        :company company
                        :channel (make-instance 'dummy-channel)
                        :merge-base "dfdfdf"
                        :commit-hash "car"))
          (check))
      (cl-mock:if-called 'push-remote-check
                         (lambda (promoter run %check)
                           (declare (ignore promoter run))
                           (setf check %check)))
      (let ((run (make-recorder-run
                  :company company
                  :channel (make-instance 'dummy-channel)
                  :work-branch "master"
                  :branch "master"
                  :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                  :merge-base "car"
                  :commit-hash "foo")))
        (maybe-promote promoter run)
        (is-true check)
        (is (eql :success (check-status check)))
        (is (equal "Nothing to review" (check-title check)))))))

(test without-a-base-run-we-get-an-error
  (with-fixture state ()
    (let ((*base-run* nil))
      (let* ((image (make-image
                     :pathname (asdf:system-relative-pathname :screenshotbot "fixture/rose.png")))
             (screenshot (make-screenshot
                          :image image
                          :name "foobar"))
             (run (make-recorder-run
                   :channel (make-instance 'dummy-channel)
                   :company company
                   :screenshots (list screenshot)
                   :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                   :merge-base "car"
                   :commit-hash "foo"))
           (check))
       (cl-mock:if-called 'push-remote-check
                          (lambda (promoter run %check)
                            (declare (ignore promoter run))
                            (setf check %check)))
       (maybe-promote promoter run)
       (is-true check)
       (is (equal "car" (pr-merge-base promoter run)))
       (is (eql :action-required (check-status check)))
       (is (equal "1 added" (check-title check)))))))

(test check-result-for-diff-report
  (with-installation ()
   (with-test-store ()
     (let* ((company (make-instance 'company))
            (channel (make-instance 'channel
                                    :name "github-test-channel"))
            (empty-run (make-recorder-run :company company
                                          :channel channel))
            (another-empty-run (make-recorder-run :company company
                                                  :channel channel))
            (empty-report (make-instance 'diff-report :added nil
                                                     :deleted nil
                                                     :changes nil)))
       (let ((check (make-check-result-from-diff-report
                     (make-instance 'pull-request-promoter)
                     empty-run another-empty-run)))
         (is (eql :success (check-status check)))
         (is (equal "No screenshots changed"
                    (check-title check))))))))

(test check-result-for-unempty-diff-report
  (cl-mock:with-mocks ()
   (with-installation ()
     (with-test-store ()
       (let ((company (make-instance 'company))
             (diff-report (make-instance
                           'diff-report
                           :added nil
                           :deleted nil
                           :changes (list
                                     (make-instance
                                      'change
                                      :before (make-instance 'screenshot :name "foo")
                                      :after (make-instance 'screenshot :name "foo"))))))
         (let ((run (make-recorder-run
                                   :company company
                                   :channel (make-instance 'dummy-channel)))
               (another-run (make-recorder-run
                             :company company
                             :channel (make-instance 'dummy-channel))))
           (answer (make-diff-report run another-run)
             diff-report)

           (let ((check (make-check-result-from-diff-report
                         (make-instance 'pull-request-promoter)
                         run another-run)))
             (is (eql :action-required(check-status check)))
             (is (cl-ppcre:scan "1 change.*" (check-title check))))))))))

(test report-has-acceptable
  (with-fixture state ()
    (let ((*base-run* (make-recorder-run
                        :company company
                        :channel (make-instance 'dummy-channel)
                        :commit-hash "car")))
     (let ((run (make-recorder-run
                  :channel (make-instance 'dummy-channel)
                  :company company
                  :github-repo "https://github.com/tdrhq/fast-example"
                  :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                  :screenshots (list (make-instance 'screenshot :name "foobar"))
                  :merge-base "car"
                  :commit-hash "foo")))
       (maybe-promote promoter run)
       (is-true (the-only-report))))))

(test maybe-send-tasks-happy-path
  (with-fixture state ()
    (cl-mock:with-mocks ()
      (let (calls)
        (cl-mock:if-called 'github-update-pull-request
                           (lambda (&rest args)
                             (push args calls))
                           :at-start t)
        (setf (send-task-args promoter) '(:dummy))
        (let ((run (make-recorder-run
                    :channel (make-instance 'dummy-channel)
                    :company company
                    :github-repo "https://github.com/tdrhq/fast-example"
                    :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                    :screenshots (list (make-instance 'screenshot :name "foobar"))
                    :merge-base "car"
                    :commit-hash "foo")))
          (push-remote-check promoter run (make-check run
                                                      :status :accepted
                                                      :title "foobar"))
          (assert-that calls
                       (has-length 1)))))))

(test setf-acceptable-state-happy-path
  (with-fixture state ()
    (with-test-user (:logged-in-p t)
      (cl-mock:with-mocks ()
        (let (calls)
          (cl-mock:if-called 'github-update-pull-request
                             (lambda (&rest args)
                               (push args calls))
                             :at-start t)
          (let* ((run (make-recorder-run
                       :channel (make-instance 'dummy-channel)
                       :company company
                       :github-repo "https://github.com/tdrhq/fast-example"
                       :pull-request "https://github.com/tdrhq/fast-example/pull/2"
                       :screenshots (list (make-instance 'screenshot :name "foobar"))
                       :merge-base "car"
                       :commit-hash "foo"))
                 (report (make-instance 'report :run run
                                        :previous-run (make-recorder-run)))
                 (acceptable (make-instance 'pr-acceptable
                                            :send-task-args nil
                                            :report report)))
            (setf (acceptable-state acceptable) :accepted)
            (assert-that calls
                         (has-length 1))))))))


(test make-github-for-every-version-of-state
  (with-fixture state ()

    (dolist (state (list :accepted :rejected :success :failure :action-required))
      (let* ((channel (make-instance 'channel
                                     :name "test-channel"
                                     :github-repo "https://github.com/tdrhq/fast-example"))
             (run (make-recorder-run
                  :github-repo "https://github.com/tdrhq/fast-example"
                  :channel channel
                  :commit-hash "zoidberg"))
            (promoter (make-instance 'pull-request-promoter))
             (check (make-check run
                                :status state
                                :title "foobar")))
       (let ((result (plist-alist (make-github-args run check))))
         (is (equal "zoidberg" (assoc-value result :head-sha)))
         ;; See: https://docs.github.com/en/rest/checks/runs?apiVersion=2022-11-28
         (is (str:s-member (list "action_required" "cancelled" "failure" "neutral"
                                 "success" "skipped" "stale" "timed_out")
                           (assoc-value result :conclusion))))))))
