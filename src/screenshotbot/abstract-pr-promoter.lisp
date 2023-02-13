;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/abstract-pr-promoter
  (:use #:cl)
  (:import-from #:screenshotbot/promote-api
                #:maybe-send-tasks
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
                #:current-user
                #:user-email
                #:user-full-name
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
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/model/report
                #:acceptable-reviewer
                #:acceptable-report
                #:acceptable-state
                #:base-acceptable)
  (:import-from #:util/lparallel
                #:immediate-promise)
  (:export
   #:check
   #:check-status
   #:report
   #:check-title
   #:details-url
   #:check-summary
   #:pull-request-info
   #:retrieve-run
   #:send-task-args
   #:run-retriever
   #:valid-repo?
   #:plugin-installed?
   #:make-acceptable
   #:pr-merge-base
   #:format-updated-summary
   #:abstract-pr-promoter
   #:abstract-pr-acceptable
   #:make-promoter-for-acceptable))
(in-package :screenshotbot/abstract-pr-promoter)

(defun format-updated-summary (state user)
  (let ((summary
          (str:downcase (string state))))
    (when user
      (setf summary (format nil "~a by ~a"
                            summary
                            (or
                             (user-full-name user)
                             (user-email user)))))))

(defclass pull-request-info ()
  ())

(defclass check ()
  ((status :initarg :status
           :accessor check-status)
   (user :initarg :user
         :initform nil
         :reader check-user
         :documentation "The user who initiated this check")
   (report :initarg :report
           :initform nil
           :accessor report)
   (title :initarg :title
          :accessor check-title)
   (details-url :initarg :details-url
                :initform nil
                :accessor details-url)
   (summary :initarg :summary
            :initform "NA"
            :accessor check-summary)))

(defclass run-retriever ()
  ((sleep-fn :initarg :sleep-fn
             :reader sleep-fn
             :initform #'sleep)))


(defmethod retrieve-run ((retriever run-retriever)
                         channel
                         base-commit)
  (labels ((produce (retries)
             (let ((run (production-run-for channel :commit base-commit)))
               (cond
                 (run
                  (immediate-promise run))
                 ((>= retries 0)
                  (lparallel:future
                    (log:info "Waiting 30s before checking again")
                    (funcall (sleep-fn retriever) 30)
                    (lparallel:chain (produce (1- retries)))))))))
    (produce 10)))

(defclass abstract-pr-acceptable (base-acceptable)
  ()
  (:metaclass persistent-class))

(defclass abstract-pr-promoter (promoter)
  ((pull-request-info :accessor pull-request-info
                      :initarg :pull-request-info
                      :initform (make-instance 'pull-request-info))
   (run-retriever :accessor run-retriever
                  :initarg :run-retriever
                  :initform (make-instance 'run-retriever))
   (send-task-args :accessor send-task-args
                   :initform nil)))

(defgeneric valid-repo? (promoter repo))

(defgeneric plugin-installed? (promoter company repo-url))

(defgeneric make-acceptable (promoter report &rest args))

(defmethod pr-merge-base ((promoter abstract-pr-promoter) run)
  (recorder-run-merge-base run))

(defun make-details-url (&rest args)
  (format nil
          "~a~a"
          (installation-domain (installation))
          (apply #'hex:make-url args)))

(defgeneric push-remote-check (promoter-or-acceptable
                               run
                               check)
  (:documentation "Push the CHECK to the corresponding run remotely "))

(defgeneric make-promoter-for-acceptable (acceptable))

(defmethod (setf acceptable-state) :after (state (self abstract-pr-acceptable)
                                           &key user)
  (push-remote-check
   (make-promoter-for-acceptable self)
   (report-run (acceptable-report self))
   (make-check-for-report
    (acceptable-report self)
    :status state
    :user user)))

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

       (do-promotion-log :info "Base commit is: ~S" (pr-merge-base promoter run))
       (let ((base-run-promise (retrieve-run
                                (run-retriever promoter)
                                (recorder-run-channel run)
                                (pr-merge-base promoter run))))
         (flet ((make-failure-check (&rest args)
                  (apply #'make-instance 'check
                         :status :failure
                         :details-url (make-details-url 'screenshotbot/dashboard/run-page:run-page
                                                        :id (oid run))
                         args)))
           (unless (lparallel:fulfilledp base-run-promise)
             (do-promotion-log :info "Base commit is not available yet, waiting for upto 5 minutes")
             (push-remote-check
              promoter
              run
              (make-instance 'check
                             :status :pending
                             :details-url (make-details-url 'screenshotbot/dashboard/run-page:run-page
                                                        :id (oid run))
                             :title (format nil "Waiting for screenshots on ~a to be available"
                                            (str:substring 0 4 (pr-merge-base promoter run))))))
           (let* ((base-run (lparallel:force base-run-promise))
                  (check (cond
                           ((null (pr-merge-base promoter run))
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
             (push-remote-check promoter run check)))))
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
        (make-check-for-report
         report
         :status :action_required
         :summary "Please verify that the images look reasonable to you"))))))

(defun make-check-for-report (report &key status (summary "") user)
  (let* ((title (diff-report-title (make-diff-report
                                    (report-run report)
                                    (report-previous-run report))))
         (title (cond
                  (user
                   (format nil "~a, ~a"
                           title
                           (format-updated-summary status user)))
                  (t
                   title))))
   (make-instance
    'check
    :status status
    :user user
    :report report
    :title title
    :summary summary
    :details-url (make-details-url 'screenshotbot/dashboard/reports:report-page
                                   :id (oid report)))))

(defmethod maybe-send-tasks ((promoter abstract-pr-promoter) run)
  (values))
