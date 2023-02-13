;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/pro/bitbucket/promoter
  (:nicknames :screenshotbot/bitbucket/promoter)
  (:use #:cl
        #:screenshotbot/abstract-pr-promoter)
  (:import-from #:screenshotbot/promote-api
                #:maybe-send-tasks
                #:plugin-promoter)
  (:import-from #:screenshotbot/pro/bitbucket/plugin
                #:bitbucket-repo
                #:bitbucket-plugin)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:promoter-pull-id
                #:make-promoter-for-acceptable
                #:abstract-pr-acceptable
                #:push-remote-check
                #:format-updated-summary
                #:check-title
                #:check-summary
                #:make-acceptable
                #:details-url
                #:send-task-args
                #:check-status
                #:valid-repo?
                #:plugin-installed?)
  (:import-from #:screenshotbot/pro/bitbucket/settings
                #:get-access-token-from-refresh-token
                #:refresh-token
                #:bitbucket-settings-for-company)
  (:import-from #:screenshotbot/model/channel
                #:github-get-canonical-repo)
  (:import-from #:screenshotbot/model/recorder-run
                #:override-commit-hash
                #:recorder-run-company)
  (:import-from #:screenshotbot/user-api
                #:pull-request-url
                #:channel-repo
                #:current-user
                #:recorder-run-channel
                #:channel-name
                #:recorder-run-commit)
  (:import-from #:screenshotbot/model/report
                #:base-acceptable
                #:acceptable-state)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/report-api
                #:report-run)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/dashboard/run-page
                #:run-page)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-domain)
  (:import-from #:screenshotbot/pro/bitbucket/audit-log
                #:with-audit-log
                #:parse-error-response
                #:http-result-code
                #:audit-log-error-response
                #:audit-log-error
                #:build-status-audit-log)
  (:import-from #:util/misc
                #:not-empty!
                #:not-null!)
  (:import-from #:screenshotbot/pro/bitbucket/core
                #:http-success-response?
                #:bitbucket-error)
  (:import-from #:screenshotbot/events
                #:push-event)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:abstract-pr-promoter)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/bitbucket/promoter)

(with-class-validation
 (defclass bitbucket-acceptable (abstract-pr-acceptable)
   ((send-task-args :initarg :report
                    :accessor send-task-args)
    (%company :initarg :company
              :reader company))
   (:metaclass persistent-class)))


(defclass bitbucket-promoter (abstract-pr-promoter)
  ((plugin :initarg :plugin
           :reader plugin)))

(defmethod make-promoter-for-acceptable ((self bitbucket-acceptable))
  (make-instance 'bitbucket-promoter))

(defmethod plugin-installed? ((promoter bitbucket-promoter)
                              company
                              repo-url)
  (bitbucket-settings-for-company company))

(defmethod make-acceptable ((promoter bitbucket-promoter) report
                            &rest args)
  (apply #'make-instance
         'bitbucket-acceptable
         :company (recorder-run-company (report-run report))
         :report report
         args))

(defmethod valid-repo? ((promoter bitbucket-promoter)
                        repo)
  (typep repo 'bitbucket-repo))

(defmethod push-remote-check ((promoter bitbucket-promoter)
                              run
                              check)
  "Send the build status. Log any error message, but don't propagate the errors"
  (handler-case
      (let* ((company (recorder-run-company run))
             (bitbucket-token (not-null! (car (bitbucket-settings-for-company company))))
             (token (get-access-token-from-refresh-token
                     company
                     (refresh-token bitbucket-token)))
             (args (make-build-status-args run check)))
        (assert token)
        (let* ((commit (not-empty! (a:assoc-value args :commit)))
               (full-name (not-empty! (a:assoc-value args :full-name))))
          (with-audit-log (audit-log (make-instance 'build-status-audit-log
                                                    :company company
                                                    :commit commit
                                                    :full-name full-name))
           (let* ((url (format nil "https://api.bitbucket.org/2.0/repositories/~a/commit/~a/statuses/build/"
                               full-name
                               commit)))

             (multiple-value-bind (stream result-code)
                 (util/request:http-request
                  url
                  :method :post
                  :content-type "application/json"
                  :want-stream t
                  :additional-headers
                  `(("Authorization" . ,(Format nil "Bearer ~a" token)))
                  :force-binary nil
                  :content (json:encode-json-to-string args))
               (let ((ret (uiop:slurp-input-stream 'string stream)))
                 (cond
                   ((http-success-response? result-code)
                    (push-event :bitbucket.update-success)
                    (log:info "Got bitbucket result: ~a" ret))
                   (t ;; error
                    (push-event :bitbucket.update-failure)
                    (log:info "Got BitBucket response code: ~a" result-code)
                    (parse-error-response ret result-code audit-log)))))))))
    (bitbucket-error (e)
      (values))))

(auto-restart:with-auto-restart ()
  (defmethod maybe-send-tasks ((promoter bitbucket-promoter) run)
    (values)))


(defmethod plugin-promoter ((plugin bitbucket-plugin))
  (make-instance 'bitbucket-promoter
                  :plugin plugin))

(defun nullify (str)
  (if (str:emptyp str) nil str))

(defun make-key (channel-name)
  (let ((old-key (format nil "screenshotbot--~a" channel-name)))
    (cond
      ((<= (length old-key) 40)
       old-key)
      (t
       (ironclad:byte-array-to-hex-string (md5:md5sum-string old-key))))))

(defmethod promoter-pull-id ((promoter bitbucket-promoter) run)
  (pull-request-url run))


(defun make-build-status-args (run
                               check)
  (let* ((channel (recorder-run-channel run))
         (repo (channel-repo channel))
         (channel-name (channel-name channel)))
   (flet ((make-details-url (&rest args)
            (format nil
                    "~a~a"
                    (installation-domain (installation))
                    (apply #'hex:make-url args))))
     `((:key . ,(make-key channel-name))
       ;; TODO: refactor repo-full-name to not use GitHub specific code.
       (:full-name . ,(screenshotbot/github/pull-request-promoter::repo-full-name repo))
       (:commit . ,(or
                    (nullify (override-commit-hash run))
                    (recorder-run-commit run)))
       (:state . ,(ecase (check-status check)
                    (:success "SUCCESSFUL")
                    (:failure "FAILED")
                    (:accepted "SUCCESSFUL")
                    (:rejected "FAILED")
                    (:pending "INPROGRESS")
                    (:action_required "FAILED")
                    (:action-required "FAILED")))
       (:name . ,(format nil "Screenshots for ~a" channel-name))
       (:url . ,(or (details-url check)
                    (make-details-url 'run-page :id (oid run))))
       (:description . ,(check-title check))))))
