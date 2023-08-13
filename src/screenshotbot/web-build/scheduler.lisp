;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/web-build/scheduler
  (:use #:cl)
  (:nicknames :screenshotbot/pro/web-build/scheduler)
  (:import-from #:screenshotbot/web-build/project
                #:web-project-scheduled-job
                #:update-next-job-at
                #:web-project-name
                #:web-project-schedule-p
                #:actually-run-now
                #:company
                #:web-project-schedule-every
                #:next-job-at
                #:web-project)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/model/company
                #:company-admins
                #:company-owner)
  (:import-from #:screenshotbot/server
                #:*domain*)
  (:import-from #:screenshotbot/installation
                #:installation
                #:installation-domain)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:util/cron
                #:def-cron)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:update-next-job-at))
(in-package :screenshotbot/web-build/scheduler)

(defun start-ts (id)
  (mod
   (reduce
    (lambda (a b) (+ b (* 256 a)))
    (md5:md5sum-string (format nil "~a" id))
    :initial-value 0)
   (* 24 3600)))

(defvar *lock* (bt:make-lock "scheduler"))

(defun next-runtime (now start-ts schedule)
  "Find the lowest start-ts + k * schedule * 3600 that's greater than
   now"
  (assert (> schedule 0))
  (let ((k (ceiling (- now start-ts) (* schedule 3600))))
    (+ start-ts (* k schedule 3600))))

(defun %run-now (project)
  (let ((company (company project)))
   (actually-run-now
    project
    :user (or
           (company-owner company)
           (car (company-admins company)))
    :company company
    :host (installation-domain (installation)))))

(defun update-next-job-at (project)
  (with-transaction ()
    (setf (next-job-at project) nil))
  (maybe-run-project project))


(with-auto-restart ()
 (defun maybe-run-project (project &key
                                     (now (get-universal-time))
                                     (start-ts nil))
   "Maybe run the project if needed. As an edge case, if the
next-runtime is not set, then it is calculated and updated."
   (unless (web-project-scheduled-job project) ;; new style
    (let ((start-ts (or start-ts
                        (start-ts (web-project-name project)))))
      (bt:with-lock-held (*lock*)
        (when (web-project-schedule-p project)
          (flet ((update-next-job-at ()
                   (let ((update (next-runtime now start-ts
                                               (web-project-schedule-every project))))
                     (with-transaction ()
                       (setf (next-job-at project)
                             update)))))
            (let ((next-job-at (next-job-at project)))
              (when (and next-job-at (< next-job-at now))
                (%run-now project))
              (update-next-job-at)))))))))


(defun run-web-projects ()
  (log:debug "Searching for web projects that are ready to run")
  (loop for project in (bknr.datastore:store-objects-with-class 'web-project) do
    (maybe-run-project project)))


(def-cron run-web-projects ()
  (run-web-projects))
