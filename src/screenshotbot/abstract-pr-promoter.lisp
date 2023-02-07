;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/abstract-pr-promoter
  (:use #:cl)
  (:import-from #:screenshotbot/promote-api
                #:maybe-promote
                #:do-promotion-log
                #:promoter)
  (:import-from #:screenshotbot/report-api
                #:report-acceptable
                #:report)
  (:import-from #:screenshotbot/model/channel
                #:production-run-for)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-commit
                #:recorder-run-channel
                #:channel-repo)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/dashboard/compare
                #:diff-report-empty-p)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:screenshotbot/diff-report
                #:make-diff-report
                #:diff-report-title)
  (:import-from #:core/installation/installation
                #:installation-domain)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/git-repo
                #:repo-link)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-merge-base
                #:recorder-run-company)
  (:export
   #:check
   #:check-status
   #:report
   #:check-title
   #:details-url
   #:check-summary
   #:base-commit
   #:pull-request-info
   #:retrieve-run
   #:send-task-args
   #:run-retriever
   #:valid-repo?
   #:plugin-installed?
   #:make-acceptable
   #:make-task-args))
(in-package :screenshotbot/abstract-pr-promoter)

(defclass pull-request-info ()
  ())

(defclass check ()
  ((status :initarg :status
           :accessor check-status)
   (report :initarg :report
           :accessor report)
   (title :initarg :title
          :accessor check-title)
   (details-url :initarg :details-url
                :initform nil
                :accessor details-url)
   (summary :initarg :summary
            :initform nil
            :accessor check-summary)))

(defclass run-retriever ()
  ())


(defmethod retrieve-run ((retriever run-retriever)
                         channel
                         base-commit)
  ;; TODO: try for a few minutes
  (production-run-for channel :commit base-commit))



(defclass abstract-pr-promoter (promoter)
  ((report :accessor report
           :initform nil)
   (base-commit :accessor base-commit
               :initform nil)
   (pull-request-info :accessor pull-request-info
                      :initarg :pull-request-info
                      :initform (make-instance 'pull-request-info))
   (run-retriever :accessor run-retriever
                  :initarg :run-retriever
                  :initform (make-instance 'run-retriever))
   (send-task-args :accessor send-task-args
                   :initform nil)))

(defgeneric valid-repo? (promoter repo))

(defgeneric plugin-installed? (promoter company repo-url))

(defgeneric make-acceptable (promoter report))

(defgeneric make-task-args (promoter
                            run
                            repo
                            check))

(defmethod maybe-promote ((promoter abstract-pr-promoter)
                          run)
  (let* ((repo (channel-repo (recorder-run-channel run)))
         (repo-url (repo-link repo))
         (company (recorder-run-company run)))
    (cond
      ((and
        (valid-repo? promoter repo)
        (plugin-installed? promoter company repo-url)
        (not (equal (recorder-run-merge-base run)
                    (recorder-run-commit run))))
       (setf (base-commit promoter)
             (recorder-run-merge-base run))

       (do-promotion-log :info "Base commit is: ~S" (base-commit promoter))
       (let ((base-run (retrieve-run
                        (run-retriever promoter)
                        (recorder-run-channel run)
                        (base-commit  promoter))))
         (let ((check (cond
                        ((null (base-commit promoter))
                         (make-instance 'check
                                        :status :failure
                                        :title "Base SHA not available for comparison, please check CI setup"
                                        :summary "Screenshots unavailable for base commit, perhaps the build was red? Try rebasing."))
                        ((null base-run)
                         (make-instance 'check
                                        :status :failure
                                        :title "Cannot generate Screenshotbot report, try rebasing"
                                        :summary "Screenshots unavailable for base commit, perhaps the build was red? Try rebasing."))
                        (t
                         (make-check-result-from-diff-report
                          promoter
                          (make-diff-report run base-run)
                          run
                          base-run)))))
           (setf (send-task-args promoter)
                 (make-task-args promoter
                                 run
                                 repo
                                 check)))

         (let ((send-task-args (send-task-args promoter)))
           (when (report promoter)
             (with-transaction ()
               (setf
                (send-task-args
                 (report-acceptable (report promoter)))
                send-task-args))))))
      (t
       #+nil
       (cerror "continue" "not promoting for ~a" promoter)
       (log:info "Initial checks failed, not going through pull-request-promoter")))))


(defun make-check-result-from-diff-report (promoter diff-report run base-run)
  (flet ((make-details-url (&rest args)
           (format nil
                   "~a~a"
                   (installation-domain (installation))
                   (apply #'hex:make-url args))))
   (cond
     ((diff-report-empty-p diff-report)
      (make-instance 'check
                      :status :success
                      :title "No screenshots changed"
                      :summary "No action required on your part"
                      :details-url
                      (make-details-url 'screenshotbot/dashboard/run-page:run-page
                                         :id (oid run))))
     (t
      (let ((report (make-instance 'report
                                    :run run
                                    :previous-run base-run
                                    :channel (when run (recorder-run-channel run))
                                    :title  (diff-report-title diff-report))))
        (with-transaction ()
          (setf (report-acceptable report)
                (make-acceptable promoter report)))
        (with-transaction ()
          (setf (report promoter)
                report))
        (make-instance 'check
                        :status :action_required
                        :title (diff-report-title diff-report)
                        :summary "Please verify that the images look reasonable to you"
                        :details-url (make-details-url 'screenshotbot/dashboard/reports:report-page
                                                        :id (oid report))))))))
