;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/pro/bitbucket/promoter
  (:nicknames :screenshotbot/bitbucket/promoter)
  (:use #:cl)
  (:import-from #:screenshotbot/promote-api
                #:maybe-send-tasks
                #:plugin-promoter)
  (:import-from #:screenshotbot/pro/bitbucket/plugin
                #:bitbucket-repo
                #:bitbucket-plugin)
  (:import-from #:screenshotbot/github/pull-request-promoter
                #:format-updated-summary
                #:check-title
                #:check-summary
                #:make-acceptable
                #:pr-acceptable
                #:details-url
                #:send-task-args
                #:check-status
                #:make-task-args
                #:valid-repo?
                #:plugin-installed?
                #:pull-request-promoter)
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
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/bitbucket/promoter)

(defclass bitbucket-acceptable (base-acceptable)
  ((send-task-args :initarg :report
                   :accessor send-task-args)
   (%company :initarg :company
             :reader company))
  (:metaclass persistent-class))


(defclass bitbucket-promoter (pull-request-promoter)
  ((plugin :initarg :plugin
           :reader plugin)))

(defmethod plugin-installed? ((promoter bitbucket-promoter)
                              company
                              repo-url)
  (bitbucket-settings-for-company company))

(defmethod make-acceptable ((promoter bitbucket-promoter) report)
  (make-instance 'bitbucket-acceptable
                  :company (recorder-run-company (report-run report))
                  :report report))

(Defmethod (setf acceptable-state) :before (state (self bitbucket-acceptable))
  (send-build-status
   (company self)
   (remove-duplicates
    (list*
     (cons :state (ecase state
                    (:accepted "SUCCESSFUL")
                    (:rejected "FAILED")))
     (cons :description
           (format nil "~a, ~a"
                   (a:assoc-value (send-task-args self) :description)
                   (format-updated-summary state (current-user))))
     (Send-task-args self))
    :from-end t
    :key #'car)))


(defmethod valid-repo? ((promoter bitbucket-promoter)
                        repo)
  (typep repo 'bitbucket-repo))

(defun send-build-status (company args)
  "Send the build status. Log any error message, but don't propagate the errors"
  (handler-case
      (let* ((bitbucket-token (not-null! (car (bitbucket-settings-for-company company))))
             (token (get-access-token-from-refresh-token
                     company
                     (refresh-token bitbucket-token))))
        (assert token)
        (let* ((commit (not-empty! (a:assoc-value args :commit)))
               (full-name (not-empty! (a:assoc-value args :full-name)))
               (audit-log (make-instance 'build-status-audit-log
                                          :company company
                                          :commit commit
                                          :full-name full-name)))
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
                   (parse-error-response ret result-code audit-log))))))))
    (bitbucket-error (e)
      (values))))

(auto-restart:with-auto-restart ()
  (defmethod maybe-send-tasks ((promoter bitbucket-promoter) run)
    (let ((company (recorder-run-company run)))
      (a:when-let (args (send-task-args promoter))
       (send-build-status company
                          args)))))

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

(defmethod make-task-args ((promoter bitbucket-promoter)
                           run
                           full-name
                           check)
  (let ((channel-name (channel-name (recorder-run-channel run))))
   (flet ((make-details-url (&rest args)
            (format nil
                    "~a~a"
                    (installation-domain (installation))
                    (apply #'hex:make-url args))))
     `((:key . ,(make-key channel-name))
       (:full-name . ,full-name)
       (:commit . ,(or
                    (nullify (override-commit-hash run))
                    (recorder-run-commit run)))
       (:state . ,(ecase (check-status check)
                    (:success "SUCCESSFUL")
                    (:failure "FAILED")
                    (:action_required "FAILED")))
       (:name . ,(format nil "Screenshots for ~a" channel-name))
       (:url . ,(or (details-url check)
                    (make-details-url 'run-page :id (oid run))))
       (:description . ,(check-title check))))))
