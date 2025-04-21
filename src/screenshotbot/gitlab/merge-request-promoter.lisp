;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/gitlab/merge-request-promoter
  (:use #:cl
        #:alexandria
        #:screenshotbot/promote-api
        #:screenshotbot/abstract-pr-promoter
        #:util/java
        #:screenshotbot/model/channel
        #:screenshotbot/compare
        #:screenshotbot/model/report
        #:screenshotbot/model/recorder-run
        #:screenshotbot/gitlab/repo
        #:bknr.datastore)
  (:nicknames
   :screenshotbot/pro/gitlab/merge-request-promoter) ;; for bknr
  (:import-from #:screenshotbot/model/report
                #:base-acceptable)
  (:import-from #:screenshotbot/gitlab/repo
                #:*gitlab-url*
                #:repo-access-token)
  (:import-from #:screenshotbot/diff-report
                #:diff-report-title)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-domain)
  (:import-from #:screenshotbot/user-api
                #:current-user
                #:current-company)
  (:import-from #:screenshotbot/gitlab/settings
                #:enable-webhooks-p
                #:gitlab-request
                #:gitlab-settings
                #:company
                #:gitlab-token
                #:gitlab-url
                #:gitlab-settings-for-company)
  (:import-from #:screenshotbot/dashboard/reports
                #:report-link)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:make-promoter-for-acceptable
                #:abstract-pr-acceptable
                #:push-remote-check
                #:format-updated-summary
                #:valid-repo?
                #:send-task-args
                #:check-status
                #:details-url
                #:check-title
                #:make-acceptable
                #:plugin-installed?
                #:pull-request-promoter)
  (:import-from #:screenshotbot/gitlab/plugin
                #:gitlab-plugin)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-page)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:screenshotbot/audit-log
                #:with-audit-log)
  (:import-from #:screenshotbot/gitlab/audit-logs
                #:update-status-audit-log)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-repo-url
                #:gitlab-merge-request-iid)
  (:import-from #:screenshotbot/webhook/webhook
                #:send-webhook)
  (:import-from #:screenshotbot/gitlab/webhook
                #:gitlab-update-build-status-payload)
  (:export
   #:merge-request-promoter
   #:gitlab-acceptable))
(in-package :screenshotbot/gitlab/merge-request-promoter)


(defclass merge-request-promoter (abstract-pr-promoter)
  ((comments :initform nil
             :accessor comments)
   (report :initform nil
           :accessor promoter-report)
   (plugin :initarg :plugin
           :reader plugin)))

(defmethod plugin-installed? ((promoter merge-request-promoter)
                              company
                              repo-url)
  (alexandria:when-let ((plugin (gitlab-settings-for-company company)))
    (and
     (gitlab-url plugin)
     (or
      (not (str:emptyp (gitlab-token plugin)))
      (enable-webhooks-p plugin)))))

(defun safe-get-mr-id (run)
  (let ((mr-id (format nil "~a" (gitlab-merge-request-iid run))))
    (unless (str:emptyp mr-id)
      (parse-integer mr-id))))

(defclass merge-request ()
  ((base-sha :initarg :base-sha
             :accessor base-sha)))

(defun get-merge-request (run)
  (let ((mr-id (gitlab-merge-request-iid run)))
    (when mr-id
      (let ((project-path (project-path (recorder-run-repo-url run)))
            (mr-id (safe-get-mr-id run)))
        (when mr-id
          (let ((res (json:decode-json-from-string
                      (gitlab-request (recorder-run-company run)
                                      (format nil "/projects/~a/merge_requests/~a"
                                              (urlencode:urlencode project-path)
                                              mr-id)))))
            (make-instance 'merge-request
                           :base-sha (assoc-value (assoc-value res :diff--refs) :base--sha ))))))))

(defmethod promoter-pull-id ((promoter merge-request-promoter) run)
  (gitlab-merge-request-iid run))

(defun comment (promoter message)
  (push message (comments promoter)))

(with-class-validation
 (defclass gitlab-acceptable (abstract-pr-acceptable)
   ((report :initarg :report
            :accessor acceptable-report)
    (company :initarg :company
             :accessor acceptable-company)
    (send-task-args :accessor send-task-args)
    (discussion-id :accessor discussion-id))
   (:metaclass bknr.datastore:persistent-class)))

(defmethod make-acceptable((promoter merge-request-promoter) report
                           &rest args)
  (apply #'make-instance 'gitlab-acceptable
         :company (recorder-run-company (report-run report))
         :report report
         args))

(auto-restart:with-auto-restart (:retries 4)
  (defun post-build-status (&key
                              company
                              project-path
                              sha
                              state
                              (name
                               "Screenshotbot")
                              target-url
                              description)
    (assert
     (str:s-member (str:split ", " "pending, running, success, failed, canceled")
                   state))
    (with-audit-log (audit-log (make-instance 'update-status-audit-log
                                              :company company
                                              :commit sha))
      (declare (ignore audit-log))
      (gitlab-request company
                      (format nil "/projects/~a/statuses/~a"
                              (urlencode:urlencode project-path)
                              sha)
                      :method :post
                      :content `(("name" . ,name)
                                 ("target_url" . ,target-url)
                                 ("state" . ,state)
                                 ("description" . ,description))))))

(defmethod make-promoter-for-acceptable ((self gitlab-acceptable))
  (make-instance 'merge-request-promoter))

(defmethod valid-repo? ((promoter merge-request-promoter)
                        repo)
  (typep repo 'gitlab-repo))


(defmethod make-gitlab-args (run
                             check)
  (let ((project-path (project-path (recorder-run-repo-url run))))
   (list
    :company (recorder-run-company run)
    :project-path project-path
    :sha (check-sha check)
    :state (ecase (check-status check)
             (:success "success")
             (:accepted "success")
             (:rejected "failed")
             (:pending "pending")
             (:failure "failed")
             (:action-required "failed"))
    :target-url (or
                 (details-url check)
                 (format nil "~a~a"
                         (installation-domain (installation))
                         (hex:make-url 'run-page :id (oid run))))
    :description (check-title check))))

(defmethod push-remote-check ((promoter merge-request-promoter)
                              run check)
  (let* ((args (make-gitlab-args run check))
         (company (recorder-run-company run))
         (settings (gitlab-settings-for-company company)))
    (when (gitlab-token settings)
      (apply #'post-build-status args))
    (when (enable-webhooks-p settings)
      (send-webhook
       company
       (apply #'make-instance 'gitlab-update-build-status-payload
              args)))))

(auto-restart:with-auto-restart ()
  (defmethod maybe-send-tasks ((promoter merge-request-promoter) run)
    (values)))


(register-promoter 'merge-request-promoter)
