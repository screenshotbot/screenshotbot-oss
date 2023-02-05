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
                #:user-updated-check-run
                #:updated-check-run)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:abstract-pr-promoter)
  (:export
   #:pull-request-promoter
   #:pr-acceptable
   #:format-updated-summary))
(in-package :screenshotbot/github/pull-request-promoter)
(in-package :screenshotbot.pr)

(defclass pr-acceptable (base-acceptable)
  ((send-task-args :accessor send-task-args
                   :initarg :send-task-args))
  (:metaclass persistent-class))

(defun format-updated-summary (state user)
  (let ((summary
          (str:downcase (string state))))
    (when user
      (setf summary (format nil "~a by ~a"
                            summary
                            (or
                             (user-full-name user)
                             (user-email user)))))))

(defmethod (setf acceptable-state) :before (state (acceptable pr-acceptable))
  (let ((old-output (assoc-value (plist-alist (send-task-args acceptable)) :output)))
    (let ((summary (format-updated-summary state (current-user))))
      (with-audit-log (audit-log (make-instance 'user-updated-check-run
                                                :user (current-user)                                                :company
                                                (report-company (acceptable-report acceptable))
                                                :commit (recorder-run-commit
                                                         (report-run
                                                          (acceptable-report acceptable)))))
        (declare (ignore audit-log))
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
         (send-task-args acceptable))))))


(defclass pull-request-promoter (abstract-pr-promoter)
  ((app-id :initarg :app-id
           :accessor app-id)
   (private-key :initarg :private-key
                :accessor private-key)))

(defmethod plugin-promoter ((plugin github-plugin))
  (make-instance 'pull-request-promoter
                  :app-id (app-id plugin)
                  :private-key (private-key plugin)))

(defmethod plugin-installed? ((promoter pull-request-promoter)
                               company
                              repo-url)
  (cond
    ((not (verified-repo-p repo-url company))
     (do-promotion-log :error "Repo ~a not verified" repo-url)
     nil)
    ((not (app-installed-p (repo-string-identifier repo-url)))
     (do-promotion-log :error "The Screenshotbot app is not installed on ~a" repo-url))
    (t t)))

(defmethod valid-repo? ((promoter pull-request-promoter)
                        repo)
  (typep repo 'github-repo))

(defmethod repo-full-name (repo)
  (multiple-value-bind (full parts)
      (cl-ppcre:scan-to-strings "^https://.*/(.*/.*)$"
                                (github-get-canonical-repo
                                 (repo-link repo)))
    (assert full)
    (elt parts 0)))


(defmethod make-task-args :before (promoter
                                   run
                                   (repo string)
                                   check)
  ;; This is to ensure that all tests are using the proper repo
  ;; object, and not a string.
  (error "We expect repo to a repo object"))

(defmethod maybe-promote ((promoter pull-request-promoter) run)
  (call-next-method))

(defmethod make-task-args ((promoter pull-request-promoter)
                           run
                           (repo github-repo)
                           check)
  (let ((repo-url (repo-link (channel-repo (recorder-run-channel run))))
        (full-name (repo-full-name repo)))
    (list :app-id (app-id promoter)
          :private-key (private-key promoter)
          :full-name full-name
          :check-name (format nil "Screenshotbot Changes: ~a "
                              (channel-name (recorder-run-channel run)))
          :output `(("title" . ,(check-title check))
                    ("summary" . ,(check-summary check)))
          :status :completed
          :details-url (details-url check)
          :installation-id (app-installation-id (repo-string-identifier repo-url))
          :conclusion (str:downcase (check-status (promoter-result promoter)))
          :head-sha (or
                     (override-commit-hash run)
                     (recorder-run-commit run)))))


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
        (with-audit-log (updated-check-run
                         (make-instance 'updated-check-run
                                        :company (recorder-run-company run)
                                        :commit (recorder-run-commit run)))
          (declare (ignore updated-check-run))
          (apply 'github-update-pull-request
                 (send-task-args promoter))))
    (retry-pull-request-code ()
      (maybe-send-tasks promoter run))))
