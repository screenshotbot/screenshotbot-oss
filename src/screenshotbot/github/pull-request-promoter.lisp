;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/pull-request-promoter
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
        #:screenshotbot/model/company
        #:screenshotbot/abstract-pr-promoter)
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
  (:import-from #:screenshotbot/github/settings
                #:verified-repo-p)
  (:import-from #:screenshotbot/github/app-installation
                #:app-installation-id
                #:app-installed-p)
  (:import-from #:screenshotbot/github/access-checks
                #:repo-string-identifier)
  (:import-from #:screenshotbot/audit-log
                #:with-audit-log)
  (:import-from #:screenshotbot/model/report
                #:acceptable-report
                #:report-company)
  (:import-from #:screenshotbot/github/audit-log
                #:updated-check-run-check
                #:user-updated-check-run
                #:updated-check-run)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:unreviewable-run-p
                #:check-key
                #:check-sha
                #:make-promoter-for-acceptable
                #:check-user
                #:push-remote-check
                #:abstract-pr-acceptable
                #:abstract-pr-promoter)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/github/plugin
                #:github-plugin)
  (:import-from #:screenshotbot/user-api
                #:pull-request-url)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-work-branch)
  (:import-from #:util/logger
                #:format-log)
  (:import-from #:screenshotbot/batch-promoter
                #:emoticon-for-status)
  (:export
   #:pull-request-promoter
   #:pr-acceptable
   #:format-updated-summary))
(in-package :screenshotbot/github/pull-request-promoter)

(named-readtables:in-readtable markup:syntax)

(with-class-validation
 (defclass pr-acceptable (abstract-pr-acceptable)
   ((send-task-args :accessor send-task-args
                    :initarg :send-task-args))
   (:metaclass persistent-class)))


(defclass pull-request-promoter (abstract-pr-promoter)
  ())

(defmethod plugin-promoter ((plugin github-plugin))
  (make-instance 'pull-request-promoter))

(defmethod make-promoter-for-acceptable ((self pr-acceptable))
  (make-instance 'pull-request-promoter))

(defmethod plugin-installed? ((promoter pull-request-promoter)
                               company
                              repo-url)
  (cond
    ((not (verified-repo-p repo-url company))
     (log:info "Repo ~a not verified" repo-url)
     nil)
    ((not (app-installed-p (repo-string-identifier repo-url)))
     (log:info "The Screenshotbot app is not installed on ~a" repo-url)
     nil)
    (t t)))

(defmethod valid-repo? ((promoter pull-request-promoter)
                        repo)
  (typep repo 'github-repo))

(defmethod repo-full-name (repo)
  (repo-full-name (repo-link repo)))

(defmethod repo-full-name ((repo-link string))
  (multiple-value-bind (full parts)
      (cl-ppcre:scan-to-strings "^https://.*/(.*/.*)$"
                                (github-get-canonical-repo
                                 repo-link))
    (assert full)
    (elt parts 0)))


(defmethod unreviewable-run-p ((promoter pull-request-promoter) (run recorder-run))
  (or
   (has-merge-queue-branch run)
   (call-next-method)))

(defun has-merge-queue-branch (run)
  (let ((branch (recorder-run-work-branch run)))
    (and branch
         (or
          (str:starts-with-p "gh-readonly-queue/" branch)
          ;; For ease of testing merge queue behavior:
          (str:starts-with-p "sbfake-gh-readonly-queue-" branch)))))

;; TODO: delete
(defmethod maybe-promote ((promoter pull-request-promoter) run)
  (call-next-method))

(auto-restart:with-auto-restart (:retries 5)
  (defmethod push-remote-check ((promoter pull-request-promoter)
                                run check)
    (let ((args (make-github-args run check))
          (audit-log-args (list
                           :company (recorder-run-company run)
                           :commit (check-sha check))))
      (with-audit-log (updated-check-run
                       (trivia:match (check-user check)
                         (nil
                          (apply #'make-instance 'updated-check-run
                                 audit-log-args))
                         (user
                          (apply #'make-instance 'user-updated-check-run
                                 :user user
                                 audit-log-args))))
        (setf (updated-check-run-check updated-check-run) check)
        (apply #'github-update-pull-request args)))))

(defmethod promoter-pull-id ((promoter pull-request-promoter)
                             run)
  (pull-request-url run))


(defun make-github-summary (check)
  (markup:write-html
   ;; CAUTION: EMPTY LINES MATTER IN THIS CODE, else GitHub won't parse
   ;; the table correctly.
   <table>
     <tr>
       <td>
         ,(emoticon-for-status (check-status check))
       </td>
       <td>
         <a href= (details-url check) >
           ,(check-key check)
         </a>
       </td>
       <td>
         ,(check-title check)
       </td>
     </tr>
   </table>))

(defun make-github-args (run check)
  (let* ((repo-url (recorder-run-repo-url run))
         (full-name (repo-full-name repo-url))
         (github-plugin (github-plugin)))
    (list :app-id (app-id github-plugin)
          :private-key (private-key github-plugin)
          :full-name full-name
          :check-name (format nil "Screenshotbot Changes: ~a "
                              (check-key check))
          :output `(("title" . ,(check-title check))
                    ("summary" . ,(make-github-summary check)))
          :details-url (details-url check)
          :status (if (eql (check-status check) :pending)
                      :in-progress
                      :completed)
          :installation-id (app-installation-id (repo-string-identifier repo-url))
          :conclusion (unless (eql (check-status check) :pending)
                        (ecase (check-status check)
                          (:accepted "success")
                          (:rejected "failure")
                          (:success "success")
                          (:failure "failure")
                          (:action-required "action_required")))
          :head-sha (check-sha check))))


(defmethod make-acceptable ((promoter pull-request-promoter) report
                            &rest args)
  (apply #'make-instance 'pr-acceptable
         :report report
         args))

(defmethod notify-pr ((acceptable pr-acceptable)
                      &key title
                        summary
                        ))

(defmethod maybe-send-tasks ((promoter pull-request-promoter)
                             run)
  "TODO: delete"
  (values))
