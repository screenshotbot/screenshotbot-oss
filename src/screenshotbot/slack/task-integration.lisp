;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/slack/task-integration
  (:use #:cl
        #:alexandria
        #:screenshotbot/task-integration-api
        #:screenshotbot/model/channel
        #:screenshotbot/model/report
        #:screenshotbot/model/company
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/report
        #:anaphora
        #:screenshotbot/slack/core)
  (:export
   #:format-blame)
  (:import-from #:screenshotbot/dashboard/reports
                #:report-link)
  (:import-from #:screenshotbot/slack/core
                #:slack-error)
  (:import-from #:screenshotbot/model/channel
                #:channel-slack-channels)
  (:import-from #:screenshotbot/model/report
                #:report-channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-tags)
  (:import-from #:util/misc
                #:?.)
  (:import-from #:screenshotbot/model/company
                #:company-with-name)
  (:import-from #:screenshotbot/model/user
                #:user-personal-company)
  (:import-from #:screenshotbot/dashboard/compare
                #:find-commit-path-to-ancestor)
  (:import-from #:screenshotbot/user-api
                #:commit-link)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain))
(in-package :screenshotbot/slack/task-integration)

(defclass slack-task-integration (task-integration)
  ())

(register-task-integration 'slack-task-integration)

(defmethod enabledp ((inst slack-task-integration))
  (let ((company (task-integration-company inst)))
    (default-slack-config company)))

(defun render-tags (report)
  (let ((tags (recorder-run-tags (report-run report))))
   (cond
     (tags
      (format nil " (from run with tags ~a)" (str:join ", " tags)))
     (t
      ""))))

(defun find-report-path (report)
  (find-commit-path-to-ancestor
   (report-run report)
   (report-previous-run report)))

(defun format-blame (report path)
  (when-let* ((run (report-run report))
              (channel (recorder-run-channel run))
              (repo (channel-repo channel)))
    (cond
      ((eql 2 (length path))
       (let ((hash (car path)))
        (format nil " | Blames to <~a|~a>"
                (commit-link repo hash)
                (str:substring 0 8 hash))))
      ((> (length path) 2)
       (format nil " | <~a|Blame commits>"
               (quri:merge-uris
                (hex:make-url
                 "/blame/:run/to/:to"
                 :run (oid (report-run report))
                 :to (oid
                      ;; There *will* be a previous run since we found
                      ;; a path.
                      (report-previous-run report)))
                (installation-domain *installation*)))))))

(defun render-text (report
                    &key (path (find-report-path report)))
  (let ((run (report-run report))
        (first-line (format nil "Screenshots changed in *~a*~a"
                               (channel-name (report-channel report))
                               (render-tags report)))
        (second-line (format nil "<~a|~a>"
                             (report-link report)
                             (report-title report))))
    (format nil "~a~%~a~a"
            (cond
              ((?. recorder-run-work-branch run)
               (format nil "~a on *~a*" first-line (recorder-run-work-branch run)))
              (t
               first-line))
            second-line
            (or (format-blame report path) "unknown"))))

(defmethod actually-post-on-channel (channel
                                     report
                                     &key company
                                       (slack-config (default-slack-config company)))
  (unless (str:emptyp channel)
    (when-let ((token (?. access-token (?. access-token slack-config))))
      (handler-case
          (slack-post-on-channel
           :channel channel
           :company company
           :token token
           :blocks `#(
                      ,(alexandria:alist-hash-table
                        `((:type . "section")
                          (:text . (("type" . "mrkdwn")
                                    ("text" . ,(render-text report))))))))
        (slack-error (e)
          ;; the slack API error has already been logged, so we should
          ;; not propagate this.
          (log:error "Got error: ~a" e)
          (values))))))

(defmethod send-task ((inst slack-task-integration) report)
  (let ((company (task-integration-company inst))
        (seen (make-hash-table :test #'equal)))
    (assert (enabledp inst))
    (let ((it (default-slack-config company)))
      (flet ((post-on-channel (channel)
               (symbol-macrolet ((key (gethash (str:ensure-prefix "#" channel) seen)))
                 (unless key
                   (setf key t)
                   (actually-post-on-channel channel report
                                             :company company)))))
        (when (enabledp it)
          (post-on-channel (slack-config-channel it)))

        (mapc #'post-on-channel
              (channel-slack-channels
               (report-channel report)))))))

(defun test-message (report-id)
  "A function to test how a slack messages renders. This is an
integration test, and only works on Staging."
  (let ((company (user-personal-company (%u:user-with-email "arnold@tdrhq.com"))))
    (actually-post-on-channel
     "#test-channel"
     (util:find-by-oid report-id)
     :company company)))

