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
                #:channel-active-commits
                #:channel-deleted-p
                #:channel-company
                #:production-run-for)
  (:import-from #:screenshotbot/user-api
                #:channel-name
                #:created-at
                #:current-user
                #:user-email
                #:user-full-name
                #:recorder-run-commit
                #:recorder-run-channel
                #:channel-repo)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/diff-report
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
                #:commit-graph
                #:commit-graph-dag
                #:compute-merge-base
                #:get-parent-commit
                #:repo-link)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-author
                #:push-run-warning
                #:runs-for-channel
                #:recorder-run-branch-hash
                #:recorder-run-branch
                #:recorder-run-work-branch
                #:abstract-run
                #:unchanged-run
                #:override-commit-hash
                #:recorder-run-batch
                #:recorder-run
                #:merge-base-failed-warning
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
                #:when-let*
                #:when-let)
  (:import-from #:anaphora
                #:it)
  (:import-from #:screenshotbot/model/failed-run
                #:run-failed-on-commit-p)
  (:import-from #:util/logger
                #:format-log)
  (:import-from #:util/threading
                #:scheduled-future)
  (:import-from #:screenshotbot/model/finalized-commit
                #:commit-finalized-p)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:util/misc
                #:ntrim-list)
  (:import-from #:util/fset
                #:do-reverse-set)
  (:import-from #:screenshotbot/model/pr-rollout-rule
                #:disable-pull-request-checks-p
                #:pr-rollout-rule-for-company)
  (:import-from #:screenshotbot/dashboard/compare
                #:warmup-report)
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
   #:promoter-pull-id
   #:check-sha
   #:push-remote-check-via-batching
   #:+nothing-to-review+))

(in-package :screenshotbot/abstract-pr-promoter)

(defvar *logs* nil
  "See trim-logs cron job that clears these references.")

(defun print-logs (&key (substring ""))
  (loop for log in *logs*
        if (str:containsp substring (check-title (second log)))
          do
             (format t "~a: ~a (at ~a)~%"
                     (recorder-run-company (first log))
                     (check-title (second log))
                     (created-at (first log)))))

(defun total-summary-length ()
  "Compute the sum of the length of all summaries in *logs*."
  (loop for log in *logs*
        for check = (second log)
        sum (length (check-summary check))))

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
   (key :initarg :key
        :initform (error "must provide :key for check")
        :accessor check-key)
   (title :initarg :title
          :accessor check-title)
   (details-url :initarg :details-url
                :initform nil
                :accessor details-url)
   (summary :initarg :summary
            :initform "NA"
            :accessor check-summary)
   (sha :initarg :sha
        :initform (error "must provide :sha for check")
        :accessor check-sha)))

(defclass run-retriever ()
  ((sleep-time :initarg :sleep-time
               :initform 30
               :reader sleep-time)))


(defun find-last-green-run (channel commit &key (depth 100))
  (when commit
   (or
    (production-run-for channel :commit commit)
    (when (> depth 0)
      (when-let* ((repo (channel-repo channel))
                  (parent-commit (get-parent-commit repo commit)))
        (find-last-green-run
         channel
         parent-commit
         :depth (1- depth)))))))

(defmethod retrieve-run ((retriever run-retriever)
                         channel
                         base-commit
                         logger)
  (labels ((failover ()
             (immediate-promise (find-last-green-run channel base-commit)))
           (produce (base-commit retries)
             (assert (not (channel-deleted-p channel)))
             (anaphora:acond
               ((null base-commit)
                (immediate-promise nil))
               ((production-run-for channel :commit base-commit)
                (immediate-promise it))
               ((or
                 (commit-finalized-p (channel-company channel) base-commit)
                 (run-failed-on-commit-p channel base-commit))
                (failover))
               ((>= retries 0)
                (format-log logger :info "Waiting ~as before checking again for ~a"
                            (sleep-time retriever)
                            base-commit)
                (scheduled-future ((sleep-time retriever))
                  (lparallel:chain (produce base-commit (1- retries)))))
               (t
                (failover)))))
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
                   :initform nil)
   (%pr-merge-base-cache :accessor %pr-merge-base-cache
                         :initform nil)))

(defgeneric valid-repo? (promoter repo)
  (:method (promoter repo)
    nil))

(defgeneric plugin-installed? (promoter company repo-url))

(defgeneric make-acceptable (promoter report &rest args))

(defmethod %compute-pr-merge-base-from-graph ((promoter abstract-pr-promoter)
                                              (run recorder-run))
  "We currently use the merge base computed on the client and sent via
API. We're eventually going to replace it with this."
  (util:or-setf
   (slot-value promoter '%pr-merge-base-cache)
   (when-let* ((channel (recorder-run-channel run))
               (repo (channel-repo channel))
               (master-commit (recorder-run-branch-hash run))
               (this-commit
                ;; We intentionally don't use ACTUAL-SHA here. If this
                ;; commit was merged into the main branch just before
                ;; generating the screenshots, then we must use the
                ;; non-overriden COMMIT to compare the merge base.
                (recorder-run-commit run)))
     (let* ((dag (commit-graph-dag (commit-graph repo)))
            (active-commits (channel-active-commits channel))
            ;; Filter out commits that aren't in the DAG to avoid warnings
            (active-commits-in-dag (remove-if-not
                                    (lambda (commit)
                                      (dag:get-commit dag commit))
                                    active-commits)))
       (compute-merge-base repo
                           (list*
                            master-commit
                            active-commits-in-dag)
                           this-commit)))))

(defmethod pr-merge-base ((promoter abstract-pr-promoter)
                          (run recorder-run))
  (let ((computed
          (%compute-pr-merge-base-from-graph promoter run)))
    (push-event :computed-merge-base
                :run (oid run)
                :computed-value computed
                :provided-value (recorder-run-merge-base run))
    (or
     computed
     (call-next-method))))


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

(defgeneric push-remote-check-via-batching (promoter
                                            batch
                                            run
                                            check))

(defmethod push-remote-check :around (promoter
                                      (run abstract-run)
                                      check)
  (let ((batch (recorder-run-batch run)))
    (cond
      (batch
       (push-remote-check-via-batching promoter batch run check))
      (t
       (call-next-method)))))

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

(defmethod actual-sha ((run abstract-run))
  (or
   (override-commit-hash run)
   (recorder-run-commit run)))

(defmethod make-check ((run abstract-run) &rest args)
  (apply #'make-instance 'check
         :sha (actual-sha run)
         :key (channel-name (recorder-run-channel run))
         args))


(defmethod run-details-url ((run recorder-run))
  (make-details-url 'screenshotbot/dashboard/run-page:run-page
                    :id (oid run)))

(defmethod run-details-url ((run unchanged-run))
  (make-details-url "/unchanged-runs/:id" :id (bknr.datastore:store-object-id run)))

(defun make-run-check (run &rest args)
  "Creates a check that points directly to the run"
  (apply #'make-check run
         :details-url (run-details-url run)
         args))

(defmethod unreviewable-run-p (promoter (run recorder-run))
  "Check if the run is running on a main branch, or some other flow
which is not reviewable but where we still need to send check
results. (In particular, on GitHub merge-queue runs are also not
reviewable.)"
  (and (recorder-run-branch run)
       (equal (recorder-run-work-branch run)
              (recorder-run-branch run))))

(defmethod unreviewable-run-p ((promoter t) (run unchanged-run))
  t)

(defparameter +nothing-to-review+ "Nothing to review"
  "This is used by batch-promoter to change the final output by a bit.")

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
      ((unreviewable-run-p promoter run)
       ;; TODO(T1096): do we have to worry about overwriting a
       ;; previously reviewable report? This might happen if a commit
       ;; was part of a PR, and then merged into the main branch, and
       ;; the build was run both times. Perhaps in this case we should
       ;; keep a track of history of notifications?
       (format-log run :info "This run is not expected to be reviewed, will probably be handled by the master-promoter")
       (push-remote-check
        promoter
        run
        (make-run-check run :status :success
                            :title +nothing-to-review+
                            :summary "NA")))
      ((not (pr-merge-base promoter run))
       (format-log run :info "No merge base on run, this is probably a bug on the CI job (Usually missing a `git fetch origin main`"))
      ((equal (pr-merge-base promoter run)
              (actual-sha run))
       (format-log run :info "Ignoring since the merge-base is same as the commit-hash"))
      ((disable-pull-request-checks-p (pr-rollout-rule-for-company company) run)
       (push-event :rollout-blocked-pr-request :run (oid run))
       (format-log run :info "A rollout rule is preventing us from sending notifications"))
      (t
       (format-log run :info "Base commit is: ~S" (pr-merge-base promoter run))
       (let ((base-run-promise (retrieve-run
                                (run-retriever promoter)
                                (recorder-run-channel run)
                                (pr-merge-base promoter run)
                                ;; We're using the run as a logger!
                                (identity run))))
         (unless (lparallel:fulfilledp base-run-promise)
           (format-log run :info "Base commit is not available yet, waiting for upto 5 minutes")
           (push-remote-check
            promoter
            run
            ;; TODO: this can use make-run-check? For a different diff.
            (make-check run
                        :status :pending
                        :details-url (run-details-url run)
                        :title (format nil "Waiting for screenshots on ~a to be available"
                                       (str:substring 0 4 (pr-merge-base promoter run))))))
         (let* ((base-run (lparallel:force base-run-promise))
                (merge-base (pr-merge-base promoter run))
                (check (cond
                         ((null merge-base)
                          (format-log run :info "No base-commit provided in run")
                          (make-run-check
                           run
                           :status :failure
                           :title "Base SHA not available for comparison, please check CI setup"
                           :summary "Screenshots unavailable for base commit, perhaps the build was red? Try rebasing."))
                         (t
                          (format-log run :info "Base run is ~a, preparing notification from diff-report" base-run)
                          (when base-run
                            (warn-if-not-merge-base promoter run base-run))
                          (make-check-result-from-diff-report
                           promoter
                           run
                           base-run)))))
           (assert (not (channel-deleted-p (recorder-run-channel run))))
           (push-remote-check promoter run check)))))))

(defmethod warn-if-not-merge-base ((promoter abstract-pr-promoter)
                                   (run recorder-run)
                                   (base-run recorder-run))
  "If the base run we're using is not the merge-base, add a warning"
  (unless (equal (pr-merge-base promoter run)
                 (recorder-run-commit base-run))
    (push-run-warning run 'merge-base-failed-warning
                      :compared-against base-run)))

(defmethod warn-if-not-merge-base (promoter (run unchanged-run) base-run)
  (values))

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

(defmethod same-pull-request-p (promoter run previous-run)
  "Check if two runs are on the same Pull Request. "
  (flet ((emptyp (id)
           (or
            (not id)
            (equal "" id))))
   (let ((pull-id (promoter-pull-id promoter run))
         (previous-pull-id (promoter-pull-id promoter previous-run)))
     (cond
       ((and (not (emptyp pull-id))
             (not (emptyp previous-pull-id)))
        (equal previous-pull-id pull-id))
       (t
        ;; We didn't have the Pull Request URL on one of the runs. This
        ;; could be because the CI started the job before the Pull
        ;; Request was created, but it could also be a complex CI job
        ;; where the Pull Request information was not able to be
        ;; extracted. For now, we're matching by branch-name in that
        ;; case.  There's a chance that branch name might overlap
        ;; across multiple PRs, but for that to actually be an issue we
        ;; also have to have the screenshots overlapping across those
        ;; two PRs. And even then that's only when the PR URL is not
        ;; available.
        (let ((branch1 (recorder-run-work-branch run))
              (branch2 (recorder-run-work-branch previous-run)))
          (unless (or
                   (str:emptyp branch1)
                   (str:emptyp branch2))
            (equal branch1 branch2))))))))

(defun %find-reusable-acceptable (promoter run previous-run)
  "Find an acceptable from the previous-run that can be re-used for this
run. If the previous-run or any of its acceptables cannot be re-used,
we return NIL."
  (flet ((p (x message)
           (format-log run :info "For ~a: ~a: Got ~a" previous-run message x)
           x))
    (and
     (not (eql previous-run run))
     (timestamp< (created-at previous-run)
                 (created-at run))
     (same-pull-request-p promoter run previous-run)
     (p (diff-report-empty-p
         (make-diff-report run previous-run))
        "Check if diff-report is empty")
     (p
      (review-status previous-run)
      "Check if report is accepted or rejected"))))

(defmethod previous-review (promoter run)
  (let ((pull-id (promoter-pull-id promoter run)))
    (format-log run :info "Looking for previous reports on ~a" pull-id)
    (let ((cut-off (timestamp- (local-time:now) 30 :day))
          (channel (recorder-run-channel run)))
      (do-reverse-set (previous-run (runs-for-channel channel))
        (cond
          ((local-time:timestamp> cut-off (created-at previous-run))
           (return nil))
          (t
           (let ((acceptable (%find-reusable-acceptable promoter run previous-run)))
             (when acceptable
               (return acceptable)))))))))

(defmethod make-check-result-from-diff-report (promoter (run unchanged-run)
                                           base-run)
  ;; TODO: technically it might be possible to generate a report at
  ;; this point, because screenshots *might* have changed from the
  ;; base-run.  But for now, we're assuming that if we're passing an
  ;; unchanged-run, then, the user has determined that there are no
  ;; changes from the base.
  (make-check run
              :status :success
              :title "No screenshots changed"
              :summary "No action required on your part"
              :details-url (run-details-url run)))

(defmethod make-check-result-from-diff-report (promoter run base-run)
  (let ((diff-report (make-diff-report run base-run)))
   (cond
     ((diff-report-empty-p diff-report)
      (make-check run
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
        (warmup-report report)
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
             :status :action-required
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
   (make-check (report-run report)
               :status status
               :user user
               :report report
               :title title
               :summary summary
               :details-url (make-details-url 'screenshotbot/dashboard/reports:report-page
                                              :id (oid report)))))

(defmethod maybe-send-tasks ((promoter abstract-pr-promoter) run)
  (values))

(defmethod maybe-promote :around ((promoter abstract-pr-promoter) (run unchanged-run))
  (when (recorder-run-batch run)
    (log:info "calling promotion for unchanged-run")
    (call-next-method)))

(def-cron trim-jobs (:minute 0 :step-hour 1)
  (ntrim-list *logs* 1000))
