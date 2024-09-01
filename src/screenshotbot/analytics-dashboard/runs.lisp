;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/analytics-dashboard/runs
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-channel
                #:screenshot-name
                #:created-at)
  (:import-from #:screenshotbot/model/recorder-run
                #:run-screenshot-map
                #:runs-for-company)
  (:import-from #:core/active-users/active-users
                #:format-date)
  (:import-from #:screenshotbot/model/company
                #:sub-companies-of)
  (:local-nicknames (:screenshot-map #:screenshotbot/model/screenshot-map)))
(in-package :screenshotbot/analytics-dashboard/runs)

(defun runs-for-last-60-days (company)
  "Find all the runs in the last 60 days for the given company"

  (append

   ;; Bring children too
   (loop for company in (fset:convert 'list (sub-companies-of company))
         appending (runs-for-last-60-days company))

   (let ((start-time (local-time:timestamp-
                      (local-time:now)
                      60 :day))
         (all-runs (runs-for-company company))
         (res nil))
     (loop for next = (fset:greatest all-runs)
           if (not next)
             return res
           else if (local-time:timestamp< (created-at next)
                                          start-time)
                  return res
           else do
             (push next res)
             (setf all-runs (fset:less all-runs next))))))

(defstruct active-screenshot-key
  date screenshot-key)

(defun fast-remove-duplicates (list)
  (let ((hash-table (make-hash-table :test #'equalp)))
    (loop for x in list
          do (setf (gethash x hash-table) t))
    (alexandria:hash-table-keys hash-table)))

(defun active-screenshot-keys (company)
  (let ((res))
    (loop for run in (runs-for-last-60-days company)
          for date = (format-date (local-time:timestamp-to-universal (created-at run)))
          for screenshot-map = (run-screenshot-map run)
          do
             (fset:do-map (k v (screenshot-map:to-map screenshot-map))
               (declare (ignore v))
               (push
                (make-active-screenshot-key
                 :date date
                 :screenshot-key (list (recorder-run-channel run) (screenshot-name k)))
                res)))
    (fast-remove-duplicates res)))

;; (active-screenshot-keys (screenshotbot/model/company:company-with-name "Kickie10"))
