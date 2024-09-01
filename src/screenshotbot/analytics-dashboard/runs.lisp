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
  (:import-from #:screenshotbot/model/screenshot-map
                #:screenshot-map-channel)
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

(defun fast-remove-duplicates (list &key (test #'equalp))
  (let ((hash-table (make-hash-table :test test)))
    (loop for x in list
          do (setf (gethash x hash-table) t))
    (alexandria:hash-table-keys hash-table)))

(defun runs-to-date-map (runs)
  "Return a list, with keys being date, and values being list of all distinct screenshot-maps"
  (let ((map (make-hash-table :test #'equal)))
    (loop for run in runs
          for date = (format-date (local-time:timestamp-to-universal (created-at run)))
          do
             (push (run-screenshot-map run)
                   (gethash date map)))
    (loop for date being the hash-keys of map
            using (hash-value screenshot-maps)
          collect
          (list date (fast-remove-duplicates screenshot-maps :test #'eql)))))

(defun active-screenshot-keys (company)
  (let (res
        ;; For each (date,channel), a union of all the screenshot maps
        ;; on that day
        (map-unions (make-hash-table :test #'equal)))
    (loop for (date screenshot-maps) in (runs-to-date-map (runs-for-last-60-days company)) do
      (loop for screenshot-map in screenshot-maps
            for key = (list date (screenshot-map-channel screenshot-map))
            do
               (setf
                (gethash key map-unions)
                (fset:map-union
                 (gethash key map-unions (fset:empty-map))
                 (screenshot-map:to-map screenshot-map)))))
    (loop for (date channel) being the hash-keys of map-unions
          using (hash-value screenshots)
          do
             (fset:do-map (k v screenshots)
               (declare (ignore v))
               (push
                (make-active-screenshot-key
                 :date date
                 :screenshot-key (list channel (screenshot-name k)))
                res)))
    res))
