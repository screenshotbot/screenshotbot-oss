;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/insights/runs
  (:use #:cl)
  (:import-from #:screenshotbot/user-api
                #:channel-name
                #:%created-at
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
  (:import-from #:util/hash-lock
                #:with-hash-lock-held
                #:hash-lock)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:screenshotbot/insights/variables
                #:*num-days*)
  (:local-nicknames (:screenshot-map #:screenshotbot/model/screenshot-map)))
(in-package :screenshotbot/insights/runs)

(defun runs-for-last-60-days (company &key (num-days (+ 30 *num-days*))
                                        (channel-filter nil))
  "Find all the runs in the last 60 days for the given company"

  (append

   ;; Bring children too
   (loop for company in (fset:convert 'list (sub-companies-of company))
         appending (runs-for-last-60-days company :num-days num-days))

   (let* ((start-time (local-time:timestamp-
                       (local-time:now)
                       num-days :day))
          (all-runs (runs-for-company company))
          (next-rank (1- (fset:size all-runs)))
          (res nil))
     (loop for next = (and
                       (>= next-rank 0)
                       (fset:at-rank all-runs next-rank))
           if (not next)
             return res
           else if (local-time:timestamp< (created-at next)
                                          start-time)
                  return res
           else do
             (when (run-matches-channel-filter-p next channel-filter)
               (push next res))
             (decf next-rank)))))

(defun run-matches-channel-filter-p (run channel-filter)
  (or
   (null channel-filter)
   (str:starts-with-p channel-filter (channel-name (recorder-run-channel run)))))

(defstruct active-screenshot-key
  date screenshot-key)

(defun fast-remove-duplicates (list &key (test #'equalp))
  (let ((hash-table (make-hash-table :test test)))
    (loop for x in list
          do (setf (gethash x hash-table) t))
    (alexandria:hash-table-keys hash-table)))

(defvar *format-date-cache* (make-hash-table))

(defun fast-format-date (ts)
  "format-date can be surprisingly slow, and is a bottleneck."
  (util:or-setf
   (gethash ts *format-date-cache*)
   (format-date ts)))

(defvar *hash-lock* (make-instance 'hash-lock))

(defun runs-to-date-map (runs)
  "Return a list, with keys being date, and values being list of all distinct screenshot-maps"
  (let ((map (make-hash-table :test #'equal)))
    (loop for run in runs
          for date = (fast-format-date (%created-at run))
          do
             (push (run-screenshot-map run)
                   (gethash date map)))
    (loop for date being the hash-keys of map
            using (hash-value screenshot-maps)
          collect
          (list date (fast-remove-duplicates screenshot-maps :test #'eql)))))

(defun %active-screenshot-keys (company &key channel-filter)
  (let (res)
    (loop for (date screenshot-maps) in (runs-to-date-map (runs-for-last-60-days company :channel-filter channel-filter)) do
      (loop for screenshot-map in screenshot-maps
            do
               (fset:do-map (k v (screenshot-map:to-map screenshot-map))
                 (declare (ignore v))
                 (push
                  (make-active-screenshot-key
                   :date date
                   :screenshot-key (list (screenshot-map-channel screenshot-map) (screenshot-name k)))
                  res))))
    ;; The N-DAY-ACTIVE-COUNT duplicates, but it costs more. Since
    ;; this function is cached, it makes sense to remove duplicates
    ;; here.
    (fast-remove-duplicates res :test #'equal)))

(defparameter *ans-cache* (make-hash-table :test #'equal))

(defun active-screenshot-keys (company &key channel-filter)
  (with-hash-lock-held (company *hash-lock*)
    (util:or-setf
     (gethash (list company *num-days* channel-filter) *ans-cache*)
     (%active-screenshot-keys company :channel-filter channel-filter))))

(def-cron clear-ans-cache ()
  (clrhash *ans-cache*))


;; (hcl:profile (active-screenshot-keys (screenshotbot/model/company:company-with-name "Apadmi Ltd")))
