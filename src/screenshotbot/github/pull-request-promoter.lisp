;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/pull-request-promoter
  (:use #:cl
        #:alexandria
        #:screenshotbot/promote-api
        #:screenshotbot/dashboard/reports
        #:screenshotbot/model/report
        #:screenshotbot/model/recorder-run
        #:screenshotbot/github/plugin
        #:screenshotbot/model/channel
        #:screenshotbot/compare
        #:screenshotbot/github/pr-checks
        #:screenshotbot/git-repo
        #:screenshotbot/user-api
        #:screenshotbot/github/access-checks
        #:screenshotbot/github/github-installation
        #:screenshotbot/model/user
        #:screenshotbot/model/company)
  (:nicknames #:sb.pr #:screenshotbot.pr)
  (:import-from #:util #:oid)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:persistent-class)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-title)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-domain)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-page)
  (:export
   #:pull-request-promoter
   #:pr-acceptable))
(in-package :screenshotbot/github/pull-request-promoter)
(in-package :screenshotbot.pr)

(defclass pull-request-info ()
  ())

(defclass run-retriever ()
  ())

(defclass pr-acceptable (base-acceptable)
  ((send-task-args :initarg :report
               :accessor send-task-args))
  (:metaclass persistent-class))


(defmethod (setf acceptable-state) :before (state (acceptable pr-acceptable))
  (let ((old-output (assoc-value (plist-alist (send-task-args acceptable)) :output))
        (current-user (current-user)))
    (let ((summary
            (str:capitalize (string state))))
      (when current-user
        (setf summary (format nil "~a by ~a"
                              summary
                              (user-email current-user))))
     (apply
      'github-update-pull-request
      :conclusion (ecase state
                (:accepted
                 "success")
                (:rejected
                 "failure"))
      :output `(("title" .
                         ,(format
                           nil
                           "~a, ~a"
                           (assoc-value old-output "title" :test 'equal)
                           summary))
                ("summary" . ,(assoc-value old-output "summary" :test 'equal)))
      (send-task-args acceptable)))))

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

(defmethod retrieve-run ((retriever run-retriever)
                         channel
                         base-commit)
  ;; TODO: try for a few minutes
  (production-run-for channel :commit base-commit))


(defclass pull-request-promoter (promoter)
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
   (result :accessor promoter-result)
   (send-task-args :accessor send-task-args
                   :initform nil)
   (app-id :initarg :app-id
           :accessor app-id)
   (private-key :initarg :private-key
                :accessor private-key)))

(defmethod plugin-promoter ((plugin github-plugin))
  (make-instance 'pull-request-promoter
                  :app-id (app-id plugin)
                  :private-key (private-key plugin)))

(defmethod plugin-installed? ((promoter pull-request-promoter)
                               company)
  (plugin-installation-id promoter company))

(defmethod plugin-installation-id ((promoter pull-request-promoter)
                        company)
  (installation-id (github-config
                    company)))

(defmethod valid-repo? ((promoter pull-request-promoter)
                        repo)
  (typep repo 'github-repo))

(defmacro p (x)
  `(let ((ret ,x))
     (log:info "For ~S got: ~a" ',x ret)
     ret))

(defmethod maybe-promote ((promoter pull-request-promoter)
                          run)
  (let ((repo (channel-repo (recorder-run-channel run)))
        (company (recorder-run-company run)))
    (unless (plugin-installed? promoter company)
      (do-promotion-log :error "No github installation id avaialble"))

    (do-promotion-log :info "Repo is of type: ~S" (type-of repo))

    (cond
      ((and
        (p (plugin-installed? promoter company))
        (p (valid-repo? promoter repo))
        (p (not (equal (recorder-run-merge-base run)
                     (recorder-run-commit run)))))
       (multiple-value-bind (full parts)
           (cl-ppcre:scan-to-strings "^https://.*/(.*/.*)$"
                                     (github-get-canonical-repo
                                      (repo-link repo)))
         (assert full)
         (let* ((full-name (elt parts 0)))
           (setf (base-commit promoter)
                 (recorder-run-merge-base run))

           (do-promotion-log :info "Base commit is: ~S" (base-commit promoter))
           (let ((base-run (retrieve-run
                            (run-retriever promoter)
                            (recorder-run-channel run)
                            (base-commit  promoter))))
             (setf (promoter-result promoter)
                   (cond
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
                       base-run))))
             (setf (send-task-args promoter)
                   (let ((check (promoter-result promoter)))
                     (make-task-args promoter
                                     run
                                     full-name
                                     check)))
             (when (report promoter)
               (with-transaction ()
                 (setf
                  (send-task-args
                   (report-acceptable (report promoter)))
                  (send-task-args promoter))))))))
      (t
       #+nil
       (cerror "continue" "not promoting for ~a" promoter)
       (log:info "Initial checks failed, not going through pull-request-promoter")))))

(defmethod make-task-args ((promoter pull-request-promoter)
                           run
                           full-name
                           check)
  (let ((company (recorder-run-company run)))
   (list :app-id (app-id promoter)
         :private-key (private-key promoter)
         :full-name full-name
         :check-name (format nil "Screenshotbot Changes: ~a "
                             (channel-name (recorder-run-channel run)))
         :output `(("title" . ,(check-title check))
                   ("summary" . ,(check-summary check)))
         :status :completed
         :details-url (details-url check)
         :installation-id (plugin-installation-id promoter company)
         :conclusion (str:downcase (check-status (promoter-result promoter)))
         :head-sha (or
                    (override-commit-hash run)
                    (recorder-run-commit run)))))

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
                      (make-details-url 'run-page
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
                        :details-url (make-details-url 'report-page
                                                        :id (oid report))))))))

(defmethod make-acceptable ((promoter pull-request-promoter) report)
  (make-instance 'pr-acceptable
                 :report report))

(defmethod notify-pr ((acceptable pr-acceptable)
                      &key title
                        summary
                        ))

(defmethod maybe-send-tasks ((promoter pull-request-promoter)
                             run)
  (restart-case
      (when (send-task-args promoter)
        (apply 'github-update-pull-request
               (send-task-args promoter)))
    (retry-pull-request-code ()
      (maybe-send-tasks promoter run))))
