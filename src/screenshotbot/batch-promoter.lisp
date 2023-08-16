;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/batch-promoter
  (:use #:cl)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:check-user
                #:check
                #:make-check
                #:push-remote-check
                #:push-remote-check-via-batching)
  (:import-from #:screenshotbot/model/batch
                #:batch
                #:batch-name
                #:batch-commit
                #:batch-item-report
                #:batch-item-run
                #:batch-item-channel
                #:find-batch-item
                #:batch-item
                #:batch-items)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-commit
                #:recorder-run-channel)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-company
                #:github-repo)
  (:import-from #:hunchentoot-extensions
                #:make-full-url)
  (:import-from #:screenshotbot/dashboard/batch
                #:batch-handler)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:core/installation/installation
                #:installation-domain)
  (:import-from #:screenshotbot/installation
                #:installation))
(in-package :screenshotbot/batch-promoter)

(defvar *lock* (bt:make-lock))

(auto-restart:with-auto-restart ()
 (defmethod push-remote-check-via-batching (promoter
                                            batch
                                            run
                                            check)
   (let ((item (bt:with-lock-held (*lock*)
                  (or
                   (find-batch-item batch :channel (recorder-run-channel run))
                   (make-instance 'batch-item
                                  :channel (recorder-run-channel run)
                                  :batch batch)))))
     (setf (batch-item-channel item) (recorder-run-channel run))
     (setf (batch-item-run item) run)
     (setf (batch-item-report item) (report check))

     (push-remote-check
      promoter
      batch
      (compute-check batch
                     :user (check-user check))))))

(defmethod compute-check ((batch batch)
                          &key user)
  (make-instance 'check
                 :sha (batch-commit batch)
                 :key (batch-name batch)
                 :user user ;; The user who initiated this check request, for audit-logs
                 :title (format nil "[experimentl] ~a" (batch-name batch))
                 :details-url (quri:render-uri
                               (quri:merge-uris
                                (hex:make-url
                                 'batch-handler
                                 :oid (oid batch))
                                (installation-domain (installation))))
                 :status :action-required
                 :summary "Summary not implemented yet"))
