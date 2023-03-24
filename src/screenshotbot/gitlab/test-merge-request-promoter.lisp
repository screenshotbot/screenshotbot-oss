;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/test-merge-request-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/gitlab/merge-request-promoter
                #:gitlab-acceptable
                #:post-build-status
                #:merge-request-promoter)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-commit
                #:recorder-run-channel
                #:channel-repo
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run-merge-base
                #:recorder-run)
  (:import-from #:screenshotbot/promote-api
                #:maybe-promote)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:promoter-pull-id
                #:retrieve-run
                #:valid-repo?
                #:plugin-installed?
                #:send-task-args)
  (:import-from #:screenshotbot/gitlab/settings
                #:gitlab-settings)
  (:import-from #:screenshotbot/gitlab/repo
                #:gitlab-repo)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/gitlab/plugin
                #:gitlab-plugin)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/model/report
                #:acceptable-state))
(in-package :screenshotbot/gitlab/test-merge-request-promoter)


(util/fiveam:def-suite)

(defvar *base-run* nil)

(defclass my-run-retriever ()
  ())

(defmethod retrieve-run ((retriever my-run-retriever)
                         channel base-commit
                         logger)
  (is (equal "aaa" base-commit))
  (lparallel:delay
   *base-run*))

(def-fixture state ()
  (with-test-store ()
    (cl-mock:with-mocks ()
     (with-installation (:installation
                         (make-instance 'installation
                                        :plugins
                                        (list
                                         (make-instance 'gitlab-plugin))))
       (let* ((company (make-instance 'company))
              (channel (make-instance 'channel
                                      :company company
                                      :github-repo "https://gitlab.com/tdrhq/fast-example"))
              (settings (make-instance 'gitlab-settings
                                       :company company
                                       :url "https://gitlab.com"
                                       :token "foobar"))
              (run (make-recorder-run :company company
                                                :commit-hash "baa"
                                                :merge-base "aaa"
                                                :channel channel
                                                :gitlab-merge-request-iid 7))
              (another-run (make-recorder-run))
              (*base-run* (make-recorder-run :company company))
              (last-build-status))
         (cl-mock:if-called 'post-build-status
                            (lambda (&rest args)
                              (setf last-build-status args)))
         (&body))))))

(test valid-repo
  (with-fixture state ()
    (is (equal "baa" (recorder-run-commit run)))
    (is (equal "aaa" (recorder-run-merge-base run)))
    (assert-that
     (channel-repo (recorder-run-channel run))
     (has-typep 'gitlab-repo))
    (is-true (valid-repo?
              (make-instance 'merge-request-promoter)
              (channel-repo (recorder-run-channel run))))))

(test plugin-installed
  (with-fixture state ()
    (is-true (plugin-installed?
              (make-instance 'merge-request-promoter)
              company
              "https://gitlab.com/tdrhq/fast-example"))))

(test maybe-promote-happy-path
  (with-fixture state ()
    (let ((promoter (make-instance 'merge-request-promoter
                                   :run-retriever (make-instance 'my-run-retriever))))
      (finishes
        (maybe-promote promoter run))
      (is (not (null last-build-status))))))


(test update-acceptable-state
  (with-fixture state ()
    (let* ((report (make-instance 'report
                                  :run run
                                  :previous-run another-run))
           (acceptable (make-instance 'gitlab-acceptable
                                      :report report)))
      (setf (acceptable-state acceptable)
            :accepted))))

(test gitlab-promoter-pull-request
  (with-fixture state ()
    (let* ((promoter (make-instance 'merge-request-promoter)))
      (is (equal 7 (promoter-pull-id promoter run))))))
