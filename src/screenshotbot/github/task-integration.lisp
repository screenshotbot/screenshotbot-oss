;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/github/task-integration
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/report
        #:screenshotbot/model/recorder-run
        #:screenshotbot/task-integration-api)
  (:import-from #:bknr.datastore
                #:with-transaction
                #:*create-issue-fn*)
  (:import-from #:screenshotbot/github/access-checks
                #:fix-github-link))
(in-package :screenshotbot/github/task-integration)

(defvar *create-issue-fn* 'github-create-issue)


(defclass github-task-integration (task-integration)
  ())

(defmethod enabledp ((inst github-task-integration))
  t)

(defmethod send-task ((inst github-task-integration) report)
  (assert (enabledp inst))
  (restart-case
      (when (create-github-issue-p (report-run report))
       (let* ((channel (report-channel report))
              (task-url
                (funcall
                 *create-issue-fn*
                 (fix-github-link (github-repo channel))
                 "Screenshots have changed"
                 (get-issue-content inst report))))
         (log:info "Got task: ~a" task-url)
         (with-transaction ()
           (setf (github-task report) (or task-url "unknown")))))
    (retry-send-task ()
      (send-task inst report))))

(register-task-integration 'github-task-integration)
