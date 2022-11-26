;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/gitlab/merge-request-promoter
  (:use #:cl
        #:alexandria
        #:screenshotbot/promote-api
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
                #:gitlab-settings
                #:company
                #:gitlab-token
                #:gitlab-url
                #:gitlab-settings-for-company)
  (:import-from #:screenshotbot/dashboard/reports
                #:report-link)
  (:import-from #:screenshotbot/github/pull-request-promoter
                #:format-updated-summary
                #:make-task-args
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
  (:export
   #:merge-request-promoter
   #:gitlab-acceptable))
(in-package :screenshotbot/gitlab/merge-request-promoter)


(named-readtables:in-readtable java-syntax)

(defclass merge-request-promoter (pull-request-promoter)
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
     (gitlab-token plugin))))


(defun safe-get-mr-id (run)
  (let ((mr-id (gitlab-merge-request-iid run)))
    (unless (str:emptyp mr-id)
      (parse-integer mr-id))))

(defun gitlab-request (repo-or-company url &key (method :get) content)
  (let* ((company (if (typep repo-or-company 'company)
                      repo-or-company
                      (company repo-or-company)))
        (settings (gitlab-settings-for-company company)))
    (util/request:http-request
     (format nil "~a/api/v4~a" (gitlab-url settings) url)
     :method method
     :additional-headers `(("PRIVATE-TOKEN" . ,(gitlab-token settings)))
     :ensure-success t
     :want-string t
     :content-type "application/json"
     :content (json:encode-json-to-string content))))

(defclass merge-request ()
  ((base-sha :initarg :base-sha
             :accessor base-sha)))

(defun get-merge-request (run)
  (let ((mr-id (gitlab-merge-request-iid run)))
    (when mr-id
      (let* ((repo (channel-repo (recorder-run-channel run))))
        (let ((project-path (project-path repo))
              (mr-id (safe-get-mr-id run)))
          (when mr-id
            (let ((res (json:decode-json-from-string
                        (gitlab-request repo
                                        (format nil "/projects/~a/merge_requests/~a"
                                                (urlencode:urlencode project-path)
                                                mr-id)))))
              (make-instance 'merge-request
                              :base-sha (assoc-value (assoc-value res :diff--refs) :base--sha )))))))))

;; TODO: cleanup
(defmethod maybe-promote ((promoter merge-request-promoter) run)
  (call-next-method)
  #+nil
  (restart-case
      (cond
        ((typep (channel-repo (recorder-run-channel run))
                'gitlab-repo)
         (let* ((mr (get-merge-request run)))
           (when mr
             (maybe-promote-mr promoter run mr))))
        (t
         (log:info "Not promoting, gitlab")))
    (restart-maybe-promote ()
      (maybe-promote promoter run))))

(defun comment (promoter message)
  (push message (comments promoter)))

(defclass gitlab-acceptable (base-acceptable)
  ((report :initarg :report
           :accessor acceptable-report)
   (company :initarg :company
            :accessor acceptable-company)
   (send-task-args :accessor send-task-args)
   (discussion-id :accessor discussion-id))
  (:metaclass bknr.datastore:persistent-class))

(defmethod make-acceptable((promoter merge-request-promoter) report)
  (make-instance 'gitlab-acceptable
                  :company (recorder-run-company (report-run report))
                  :report report))

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
                               ("description" . ,description)))))

(defmethod (setf acceptable-state) :before (state (acceptable gitlab-acceptable))
  (flet ((not-null! (x) (assert x) x))
   (let* ((report (acceptable-report acceptable))
          (run (report-run report))
          (send-task-args (send-task-args acceptable))
          (repo (channel-repo (recorder-run-channel run))))

     (let ((old-description (getf send-task-args :description))
           (summary (format-updated-summary state (current-user))))
      (apply #'post-build-status
               :state (ecase state
                        (:accepted "success")
                        (:rejected "failed"))
               :description (format nil "~a, ~a" old-description summary)
               send-task-args)))))

(defmethod valid-repo? ((promoter merge-request-promoter)
                        repo)
  (typep repo 'gitlab-repo))


(defmethod make-task-args ((promoter merge-request-promoter)
                           run
                           full-name
                           check)
  (let* ((repo (channel-repo (recorder-run-channel run))))
    (list
     :company (company repo)
     :project-path (project-path repo)
     :sha (recorder-run-commit run)
     :state (ecase (check-status check)
              (:success "success")
              (:failure "failed")
              (:action_required "failed"))
     :target-url (or
                  (details-url check)
                  (format nil "~a~a"
                          (installation-domain (installation))
                          (hex:make-url 'run-page :id (oid run))))
     :description (check-title check))))

(auto-restart:with-auto-restart ()
  (defmethod maybe-send-tasks ((promoter merge-request-promoter) run)
    (alexandria:when-let (args (send-task-args promoter))
      (apply #'post-build-status args))))


(register-promoter 'merge-request-promoter)
