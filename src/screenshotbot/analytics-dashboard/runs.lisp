;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/analytics-dashboard/runs
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name
                #:created-at)
  (:import-from #:screenshotbot/model/recorder-run
                #:run-screenshot-map
                #:runs-for-company)
  (:import-from #:core/active-users/active-users
                #:format-date)
  (:local-nicknames (:screenshot-map #:screenshotbot/model/screenshot-map)))
(in-package :screenshotbot/analytics-dashboard/runs)

(defun runs-for-last-60-days (company)
  "Find all the runs in the last 60 days for the given company"

  ;; TODO: sub-companies
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
            (setf all-runs (fset:less all-runs next)))))

(defclass active-screenshot-key ()
  ((date :initarg :date)
   (screnshot-key :initarg :screenshot-key)))

(defun active-screenshot-keys (company)
  (let ((res))
    (loop for run in (runs-for-last-60-days company)
          for date = (format-date (local-time:timestamp-to-universal (created-at run)))
          for screenshot-map = (run-screenshot-map run)
          do
             (fset:do-map (k v (screenshot-map:to-map screenshot-map))
               (declare (ignore v))
               (push
                (make-instance 'active-screenshot-key
                               :date date
                               :screenshot-key (screenshot-name k))
                res)))
    res))

;; (active-screenshot-keys (screenshotbot/model/company:company-with-name "Kickie10"))
