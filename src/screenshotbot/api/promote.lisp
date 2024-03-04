;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/api/promote
  (:use #:cl
        #:alexandria
        #:anaphora
        #:screenshotbot/promote-api
        #:screenshotbot/api/core
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/channel
        #:screenshotbot/git-repo)
  (:shadow #:log)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:store-object-id)
  (:import-from #:screenshotbot/github/access-checks
                #:fix-github-link)
  (:import-from #:screenshotbot/server
                #:register-init-hook
                #:*init-hooks*)
  (:import-from #:util/threading
                #:make-thread
                #:with-extras
                #:ignore-and-log-errors)
  (:import-from #:util/misc
                #:make-mp-hash-table)
  (:import-from #:util/logger
                #:noop-logger
                #:logger
                #:format-log)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:screenshotbot/model/recorder-run
                #:unchanged-run
                #:recorder-run-warnings
                #:not-fast-forward-promotion-warning
                #:promotion-log)
  (:import-from #:screenshotbot/model/batch
                #:finalize-batch
                #:batch)
  (:import-from #:screenshotbot/promote-api
                #:maybe-send-tasks)
  (:export
   #:with-promotion-log
   #:default-promoter)
  ;; forward decls
  (:export
   #:recorder-run-verify
   #:maybe-promote-run
   #:master-promoter
   #:%maybe-send-tasks))
(in-package :screenshotbot/api/promote)

(defvar *disable-ancestor-checks-p* nil
  "If the previous commit is not in the current master branch, future
  promotions will fail. Setting this to true will temporarily let you
  fix the promotion.")

(defvar *current-logger* nil)

(defvar *loggers* (make-mp-hash-table
                   #+lispworks
                   :weak-kind
                   #+lispworks
                   :key)
  "A mapping from runs to a util/logger:LOGGER instance")


(defun log (level message &rest args)
  (cond
    (*current-logger*
     (apply #'format-log *current-logger* level
            message args))
    (t
     (warn "Attempted to call promotion log when no promotion is running: ~a" message))))

(defmethod promotion-logger (run #| or batch! |#)
  (util:or-setf
   (gethash run *loggers*)
   (make-instance 'logger
                  :file (blob-pathname
                         (promotion-log run)))
   :thread-safe t))

(defmethod promotion-logger ((run unchanged-run))
  (make-instance 'noop-logger))

(defmethod promotion-logger ((run recorder-run))
  (call-next-method))

(defmethod format-log (run #| or batch |#
                       level message &rest args)
  (let ((logger (promotion-logger run)))
    (apply #'format-log
           logger
           level
           message
           args)))

(defun %with-promotion-log (run fn)
  (let ((*current-logger* run))
    (unwind-protect
         (progn
           ;;(format *current-promotion-stream* "BEGIN~%")
           (log :info "Beginning promotion: ~S" run)
           (funcall fn))
      (log :info "End promotion"))))

;; In order to this this, you need
;;


(defmacro with-promotion-log ((run) &body body)
  `(%with-promotion-log ,run (lambda () ,@body)))

(defun fix-github-link (repo)
  (cond
    ((str:starts-with-p "git@github.com:" repo)
     (cl-ppcre:regex-replace
      "[.]git$"
      (str:replace-all "git@github.com:" "https://github.com/" repo)
      ""))
    (t
     repo)))


(defclass master-promoter (promoter)
  ())

(register-promoter 'master-promoter)

(defmethod maybe-promote :around (promoter run)
  (with-extras (("run" run))
    (call-next-method)))

(defmethod maybe-promote ((promoter master-promoter) run)
  (maybe-promote-run run))

(defclass delegating-promoter (promoter)
  ((delegates :initarg :delegates
              :accessor delegates)))

(defclass default-promoter (delegating-promoter)
  ((delegates :initform (list-promoters))))

(defmethod maybe-promote ((promoter delegating-promoter) run)
  (restart-case
      (dolist (delegate (delegates promoter))
        (format-log run :info "Delegating to promoter ~s" delegate)
        (ignore-and-log-errors ()
          (maybe-promote delegate run)))
    (dangerous-restart-all-promotions ()
      (maybe-promote promoter run))))

(defmethod maybe-promote :before (promoter run)
  (format-log run :info "Running promoter: ~s on ~s" promoter run))

(defmethod maybe-send-tasks ((promoter delegating-promoter) run)
  (dolist (delegate (delegates promoter))
    (maybe-send-tasks delegate run)))

(defmethod channel-ancestorp ((channel channel) commit1 commit2)
  "check if COMMIT1 is an ancestor of COMMIT2"
  (log :info "Checking ancestry (channel: ~a): ~a ~a" (store-object-id channel) commit1 commit2)
  (restart-case
      (repo-ancestor-p (channel-repo channel)
                       commit1
                       commit2)
    (retry-channel-ancestor-p ()
      (channel-ancestorp channel commit1 commit2))))


(defmethod %channel-publicp ((channel channel))
  (restart-case
      (let ((publicp (public-repo-p (channel-repo channel))))
        (with-transaction ()
          (setf (publicp channel) publicp)))
    (continue ()
      nil)
    (retry ()
      (%channel-publicp channel))))

(defun maybe-promote-run (run &rest args &key channel
                                           (wait-timeout (if *disable-ancestor-checks-p*
                                                             0
                                                             1)))
  (declare (type recorder-run run)
           (optimize (debug 3) (speed 0)))
  (unless channel
    (setf channel (recorder-run-channel run)))
  (log :info "Begin image verifications for ~a" run)
  (recorder-run-verify run)

  (restart-case
      (let ((*channel-repo-overrides*
              (cons
               (cons channel (github-repo run))
               *channel-repo-overrides*)))
       (bt:with-lock-held ((channel-promotion-lock channel))
         (log :info "Inside promotion logic")
         (let ((run-branch (recorder-run-branch run)))
           (log :info "Branch on run: ~a" run-branch)
           (cond
             ((pull-request-url run)
              (log :error "Looks like there's a Pull Request attached, not promoting"))
             ((periodic-job-p run)
              (log :info "This is a periodic job, running promotions")
              (finalize-promotion
               run
               (active-run channel (master-branch channel))
               channel
               run-branch))
             ((null run-branch)
              (log :error "No branch set, not promoting"))
             (t
              (maybe-promote-run-on-branch run channel run-branch
                                           :wait-timeout wait-timeout))))))
    (retry-promote ()
      (apply 'maybe-promote-run run args))))

(defmethod %maybe-promote-run-on-branch ((run recorder-run)
                                         previous-run
                                         (channel channel)
                                         branch)
  (declare (optimize debug))
  (flet ((in-branch-p (commit sbranch)
           (declare (optimize debug))
           (let ((branch-commit
                   (cond
                     ((equal branch sbranch)
                      (recorder-run-branch-hash run))
                     (t
                      sbranch))))
            (channel-ancestorp channel commit branch-commit))))
    (log :info "previous run: ~s" previous-run)

    (when previous-run
      (log :info "Attempting switch: ~a -> ~a"
                (recorder-run-commit previous-run)
                (recorder-run-commit run)))
    (log :info "According to run master has branch-hash ~A"
              (recorder-run-branch-hash run))
    (cond
      ((not (trunkp run))
       (log :info  "not promoting run because it's not production run"))
      ((not (recorder-run-branch-hash run))
       (log :error  "branch-hash not present in run"))
      ((not (in-branch-p (recorder-run-commit run) branch))
       (log :error "The commit ~a is not in branch ~a"
                  (recorder-run-commit run)
                  branch))
      ((and previous-run
            (string= (recorder-run-commit previous-run)
                     (recorder-run-commit run)))
       (log :info "The current promoted run is already on the same commit"))
      ((and previous-run  #| TAG, see below |#
            (channel-ancestorp
             channel
             (recorder-run-commit run)
             (recorder-run-commit previous-run)))
       (log:info  "The current run is an ancestor of the previous run, skipping promotion"))
      (t
       (cond
         ((not (if previous-run
                   (eql previous-run (active-run channel branch))
                   t))
          (log :error "Another run got promoted while we were debating this one."))
         (t
          (flet ((previous-run-is-ancestor-p ()
                   (channel-ancestorp
                    channel
                    (recorder-run-commit previous-run)
                    (recorder-run-commit run)))
                 (call-finalize ()
                   (finalize-promotion run previous-run
                                       channel branch)))
            ;; Because of the check on line marked `TAG`, we know run
            ;; is not an ancestor of previous-run. If the other
            ;; direction is also not true, then we apply the warning
            (cond
              ((and previous-run (not (previous-run-is-ancestor-p)))
               (cond
                 ((< (store-object-id run)
                     (store-object-id previous-run))
                  (log :info "Not promoting this because this run was created before the previous-run"))
                 (t
                  (add-not-fast-forward-promotion-warning run)
                  (call-finalize))))
              (t
               (call-finalize))))))))))

(defun add-not-fast-forward-promotion-warning (run)
  (with-transaction ()
    (push (make-instance 'not-fast-forward-promotion-warning)
          (recorder-run-warnings run))))

(defmethod wait-for-run ((channel channel) commit
                     &optional (amount 15) (unit :minute))
  "Wait AMOUNT time (with UNIT) until there's just a production run on
  channel with commit COMMIT. At the moment we do not wait for the
  promotion to complete on this run."
  (restart-case
      (let ((end-time (local-time:timestamp+ (local-time:now) amount unit)))
        (bt:with-lock-held ((channel-lock channel))
          (loop while (local-time:timestamp<= (local-time:now) end-time)
                do
                   (anaphora:acond
                     ((production-run-for channel :commit commit)
                      (wait-until-run-complete it end-time :lock (channel-lock channel)
                                                           :cv (channel-cv channel))
                      (return t))
                     (t
                      ;; nothing to do. wait on the CV and try again
                      (log :info "Waiting for commit `~a` to be available (best effort, will not fail if it's unavailable)" commit)
                      (bt:condition-wait (channel-cv channel)
                                         (channel-lock channel)
                                         :timeout 15))))))
    (ignore-and-dont-want-for-run ()
      nil)))

(defmethod wait-until-run-complete ((run recorder-run) end-time &key lock cv)
  "Wait until END-TIME for run to be PROMOTION-COMPLETE-P. If we reach
  end time, then we just return"
  (loop while (local-time:timestamp<= (local-time:now) end-time)
        do
           (cond
             ((promotion-complete-p run)
              (return t))
             (t
              (log :info "Waiting for run ~a to be completed" run)
              (bt:condition-wait cv lock :timeout 5)))))

(defun maybe-promote-run-on-branch (run channel branch &key wait-timeout)
  (cond
    ((str:emptyp (github-repo run))
     (log :error "No repo link provided, cannot promote"))
    ((null (recorder-run-commit run))
     (log :error "No commit specified, not promoting"))
    (t
     (let ((parent-commit (get-parent-commit
                           (channel-repo channel)
                           (recorder-run-commit run))))
       (when parent-commit
         (wait-for-run channel parent-commit
                       wait-timeout :minute))
       (let* ((previous-run
                (active-run channel branch)))
         (%maybe-promote-run-on-branch
          run
          previous-run
          channel
          branch))))))

(auto-restart:with-auto-restart ()
 (defun finalize-promotion (run previous-run channel branch)
   (log :info "All checks passed, promoting run")
   (when previous-run
     (assert (equal branch
                    (recorder-run-branch run)))
     (when (or (not (recorder-previous-run run))
               (equal (master-branch channel)
                      branch))
       (with-transaction ()
         (setf (recorder-previous-run run)
               previous-run))))

   (unless (equal branch (master-branch channel))
     (with-transaction ()
       (setf (master-branch channel)
             branch)))

   (with-transaction ()
     (setf (active-run channel branch)
           run)
     (setf (github-repo channel)
           (github-repo run)))

   (let ((publicp (%channel-publicp channel)))
     (with-transaction ()
       (setf (publicp channel) publicp)))))

(defmethod finalize-batch ((self batch)))

(defmethod maybe-promote ((promoter master-promoter) (batch batch))
  ;; Do nothing!
  (values))

(defmethod maybe-promote ((promoter master-promoter) (run unchanged-run))
  ;; Do nothing!
  (values))

(defmethod maybe-send-tasks ((promoter master-promoter) (run unchanged-run))
  (values))
