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
                #:promoter)
  (:import-from #:screenshotbot/report-api
                #:report-previous-run
                #:report-run
                #:report-acceptable
                #:report)
  (:import-from #:screenshotbot/model/channel
                #:production-run-for)
  (:import-from #:screenshotbot/user-api
                #:created-at
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
                #:get-parent-commit
                #:repo-link)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run
                #:recorder-run-warnings
                #:merge-base-failed-warning
                #:channel-runs
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
                #:reports-for-run
                #:acceptable-report
                #:acceptable-state
                #:base-acceptable)
  (:import-from #:util/lparallel
                #:immediate-promise)
  (:import-from #:local-time
                #:timestamp-
                #:timestamp<)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:anaphora
                #:it)
  (:import-from #:screenshotbot/model/failed-run
                #:run-failed-on-commit-p)
  (:import-from #:util/logger
                #:format-log)
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
   #:make-promoter-for-acceptable
   #:promoter-pull-id))
(in-package :screenshotbot/abstract-pr-promoter)

(defvar *logs* nil)

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
                         base-commit
                         logger)
  (labels ((produce (base-commit retries)
             (anaphora:acond
               ((null base-commit)
                (immediate-promise nil))
               ((production-run-for channel :commit base-commit)
                (immediate-promise it))
               ((run-failed-on-commit-p channel base-commit)
                (format-log logger :info "The base commit has failed, let's look for its ancestor")
                (let ((repo (channel-repo channel)))
                  (let ((base-commit (get-parent-commit repo base-commit)))
                    (produce base-commit
                             ;; Keep the retries the same, since we
                             ;; aren't doing a timeout at this point.
                             retries))))
               ((>= retries 0)
                (lparallel:future
                  (format-log logger :info "Waiting 30s before checking again for ~a" base-commit)
                  (funcall (sleep-fn retriever) 30)
                  (lparallel:chain (produce base-commit (1- retries))))))))
    (produce base-commit 10)))

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

(defmethod push-remote-check :before (promoter run check)
  (atomics:atomic-push (list run check) *logs*))

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
      ((not (valid-repo? promoter repo))
       (format-log run :info "Not a valid repo for this promoter"))
      ((not (plugin-installed? promoter company repo-url))
       (format-log run :info "Plugin is not installed for this company/repository"))
      ((equal (recorder-run-merge-base run)
              (recorder-run-commit run))
       (format-log run :info "Ignoring since the merge-base is same as the commit-hash"))
      (t
       (format-log run :info "Base commit is: ~S" (pr-merge-base promoter run))
       (let ((base-run-promise (retrieve-run
                                (run-retriever promoter)
                                (recorder-run-channel run)
                                (pr-merge-base promoter run)
                                ;; We're using the run as a logger!
                                (identity run))))
         (flet ((make-failure-check (&rest args)
                  (apply #'make-instance 'check
                         :status :failure
                         :details-url (make-details-url 'screenshotbot/dashboard/run-page:run-page
                                                        :id (oid run))
                         args)))
           (unless (lparallel:fulfilledp base-run-promise)
             (format-log run :info "Base commit is not available yet, waiting for upto 5 minutes")
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
                  (merge-base (pr-merge-base promoter run))
                  (check (cond
                           ((null merge-base)
                            (format-log run :info "No base-commit provided in run")
                            (make-failure-check
                             :title "Base SHA not available for comparison, please check CI setup"
                             :summary "Screenshots unavailable for base commit, perhaps the build was red? Try rebasing."))
                           ((null base-run)
                            (push-event :promoter.no-base-run :oid (oid run))
                            (format-log run :info "Could not find base-run")
                            (make-failure-check
                             :title (format nil "Could not find a run for commit ~a, try rebasing" merge-base)
                             :summary "Screenshots unavailable for base commit, perhaps the build was red? Try rebasing."))
                           (t
                            (format-log run :info "Base run is available, preparing notification from diff-report")
                            (warn-if-not-merge-base promoter run base-run)
                            (make-check-result-from-diff-report
                             promoter
                             run
                             base-run)))))
             (push-remote-check promoter run check))))))))

(defmethod warn-if-not-merge-base ((promoter abstract-pr-promoter)
                                   (run recorder-run)
                                   (base-run recorder-run))
  "If the base run we're using is not the merge-base, add a warning"
  (unless (equal (pr-merge-base promoter run)
                 (recorder-run-commit base-run))
    (with-transaction ()
      (push (make-instance 'merge-base-failed-warning
                           :compared-against base-run)
            (recorder-run-warnings run)))))

(defgeneric promoter-pull-id (promoter run)
  (:documentation "Get a unique identifier identify the pull request for this run. This
might be a Pull Request URL, or a Merge Request IID, or a Phabricator
Revision. It will be tested with EQUAL"))

(defun review-status (run)
  (loop for report in (reports-for-run run)
        for acceptable = (report-acceptable report)
        if (and acceptable (acceptable-state acceptable)
                (acceptable-reviewer acceptable))
          return acceptable))

(defmethod previous-review (promoter run)
  (when (gk:check :auto-review-pr (recorder-run-company run) :default t)
    (when-let ((pull-id (promoter-pull-id promoter run)))
      (format-log run :info "Looking for previous reports on ~a" pull-id)
      (let ((cut-off (timestamp- (local-time:now) 30 :day))
            (channel (recorder-run-channel run)))
        (loop for previous-run in (channel-runs channel)
              while (timestamp< cut-off (created-at previous-run))
              for acceptable
                =
                (flet ((p (x message)
                         (format-log run :info "For ~a: ~a: Got ~a" previous-run message x)
                         x))
                  (and
                   (not (eql previous-run run))
                   (timestamp< (created-at previous-run)
                               (created-at run))
                   (equal (promoter-pull-id promoter previous-run) pull-id)
                   (p (diff-report-empty-p
                       (make-diff-report run previous-run))
                      "Check if diff-report is empty")
                   (p
                    (review-status previous-run)
                    "Check if report is accepted or rejected")))
              if acceptable
                return acceptable)))))

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
        (flet ((setup-acceptable (&rest args)
                 (with-transaction ()
                   (setf (report-acceptable report)
                         (apply #'make-acceptable promoter report
                                args)))))
          (trivia:match (previous-review promoter run)
           (nil
            (format-log run :info "Did not find any previous reviewed reports")
            (setup-acceptable)
            (make-check-for-report
             report
             :status :action_required
             :summary "Please verify that the images look reasonable to you"))
           (acceptable
            (format-log run :info "Found a previous review: ~a" acceptable)
            (setup-acceptable
             :state (acceptable-state acceptable)
             :user (acceptable-reviewer acceptable))
            (make-check-for-report
             report
             :status (acceptable-state acceptable)
             :user (acceptable-reviewer acceptable)
             :previousp t
             :summary "An identical run was previously reviewed on this Pull Request")))))))))

(defun format-check-title (title &key user previousp status)
  (cond
    (user
     (format nil "~a, ~a~a"
             title
             (if previousp "previously " "")
             (format-updated-summary status user)))
    (t
     title)))

(defun make-check-for-report (report &key status (summary "") user
                                       previousp)
  (let* ((title (diff-report-title (make-diff-report
                                    (report-run report)
                                    (report-previous-run report))))
         (title (format-check-title
                 title
                 :user user
                 :previousp previousp
                 :status status)))
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