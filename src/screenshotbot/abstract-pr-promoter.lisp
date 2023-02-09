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
                #:report-previous-run
                #:report-run
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
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:lparallel.promise
                #:future)
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
  ((sleep-fn :initarg :sleep-fn
             :reader sleep-fn
             :initform #'sleep)))


(defmethod retrieve-run ((retriever run-retriever)
                         channel
                         base-commit)
  (lparallel:future
    (loop for i from 0 to 10
          for run = (production-run-for channel :commit base-commit)
          if run
            return run
          else do
            (log:info "Waiting 30s before checking again")
            (funcall (sleep-fn retriever) 30))))



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
                            check))

(defun make-details-url (&rest args)
  (format nil
          "~a~a"
          (installation-domain (installation))
          (apply #'hex:make-url args)))

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
       (let ((base-run (lparallel:force
                        (retrieve-run
                         (run-retriever promoter)
                         (recorder-run-channel run)
                         (base-commit  promoter)))))
         (flet ((make-failure-check (&rest args)
                  (apply #'make-instance 'check
                         :status :failure
                         :details-url (make-details-url 'screenshotbot/dashboard/run-page:run-page
                                                        :id (oid run))
                         args)))
          (let ((check (cond
                         ((null (base-commit promoter))
                          (do-promotion-log :info "No base-commit provided in run")
                          (make-failure-check
                           :title "Base SHA not available for comparison, please check CI setup"
                           :summary "Screenshots unavailable for base commit, perhaps the build was red? Try rebasing."))
                         ((null base-run)
                          (push-event :promoter.no-base-run :oid (oid run))
                          (do-promotion-log :info "Could not find base-run")
                          (make-failure-check
                           :title "Cannot generate Screenshotbot report, try rebasing"
                           :summary "Screenshots unavailable for base commit, perhaps the build was red? Try rebasing."))
                         (t
                          (do-promotion-log :info "Base run is available, preparing notification from diff-report")
                          (make-check-result-from-diff-report
                           promoter
                           run
                           base-run)))))
            (setf (send-task-args promoter)
                  (make-task-args promoter
                                  run
                                  check))))

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


(defun make-check-result-from-diff-report (promoter run base-run)
  (let ((diff-report (make-diff-report run base-run)))
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
        (setf (report promoter) report)
        (make-check-for-report
         report
         :status :action_required
         :summary "Please verify that the images look reasonable to you"))))))

(defun make-check-for-report (report &key status (summary ""))
  (make-instance
   'check
   :status status
   :title (diff-report-title (make-diff-report
                              (report-run report)
                              (report-previous-run report)))
   :summary summary
   :details-url (make-details-url 'screenshotbot/dashboard/reports:report-page
                                  :id (oid report))))
