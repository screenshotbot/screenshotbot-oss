;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/reports
  (:use #:cl)
  (:import-from #:json
                #:encode-json-to-string)
  (:import-from #:screenshotbot/diff-report
                #:after
                #:before
                #:diff-report-added
                #:diff-report-changes
                #:diff-report-deleted
                #:make-diff-report)
  (:import-from #:screenshotbot/mcp/mcp
                #:capped
                #:dashboard-url
                #:def-tool
                #:obj
                #:tool-result
                #:visible-to-caller)
  (:import-from #:screenshotbot/model/channel
                #:channel-name)
  (:import-from #:screenshotbot/model/report
                #:report
                #:report-channel
                #:report-previous-run
                #:report-run
                #:report-title)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot-image
                #:screenshot-name)
  (:documentation "The fetch_report MCP tool.

Reports carry image ids rather than URLs; SCREENSHOTBOT/MCP/IMAGES turns
one into something a model can look at."))
(in-package :screenshotbot/mcp/reports)

(defparameter +max-changes+ 100
  "Cap on screenshots reported per section. Same reasoning as
+MAX-CHANNELS+: a 2000-screenshot report helps nobody.")

(defun find-report-by-id (id)
  "The report with ID, if this caller may see it.

Never signals. A model hands us whatever string it has, and a malformed
id has to come back as something it can read and correct rather than as
an internal error it can only retry."
  (visible-to-caller (ignore-errors (util:find-by-oid id 'report)) 'report))

(defun screenshot-json (screenshot)
  (let ((image (screenshot-image screenshot)))
    (obj "name" (screenshot-name screenshot)
         ;; The id rather than the URL: a report can carry hundreds of
         ;; screenshots, and a model should spend a call only on the ones
         ;; it decides to look at. fetch_image_url resolves them.
         "imageId" (when image (util:oid image)))))

(defun change-json (change)
  (obj "name" (screenshot-name (after change))
       "before" (screenshot-json (before change))
       "after" (screenshot-json (after change))))

(defun report-json (report)
  (let* ((run (report-run report))
         (previous (report-previous-run report))
         (diff-report (when (and run previous)
                        (make-diff-report run previous))))
    (multiple-value-bind (changed changed-total)
        (capped (if diff-report (diff-report-changes diff-report) nil)
                +max-changes+ #'change-json)
      (multiple-value-bind (added added-total)
          (capped (if diff-report (diff-report-added diff-report) nil)
                  +max-changes+ #'screenshot-json)
        (multiple-value-bind (deleted deleted-total)
            (capped (if diff-report (diff-report-deleted diff-report) nil)
                    +max-changes+ #'screenshot-json)
          (let ((result
                  (obj "id" (util:oid report)
                       "title" (report-title report)
                       "channel" (let ((channel (report-channel report)))
                                   (when channel (channel-name channel)))
                       "url" (dashboard-url "report" (util:oid report))
                       "run" (when run (util:oid run))
                       "previousRun" (when previous (util:oid previous))
                       "changed" changed
                       "added" added
                       "deleted" deleted)))
            ;; Truncation is stated, not implied by a short list: a model
            ;; that cannot see the cut reports a partial diff as the whole
            ;; one, which is worse than refusing to answer.
            (loop for (key total) in (list (list "changedTotal" changed-total)
                                           (list "addedTotal" added-total)
                                           (list "deletedTotal" deleted-total))
                  if total
                    do (setf (gethash key result) total))
            result))))))

(def-tool "fetch_report"
    ((id "report_id" "The report id, as it appears in a report URL"))
    "Fetch a Screenshotbot report by id, describing what changed between two runs. Returns JSON with the report metadata and, for each screenshot, the ids of the before and after images. Use fetch_image_url to turn an image id into a URL you can look at."
  (let ((report (find-report-by-id id)))
    (cond
      ((null report)
       ;; Deliberately one message for "no such report" and "not yours".
       ;; Distinguishing them would let a caller enumerate which report
       ;; ids exist.
       (tool-result
        (format nil "No report ~a is visible to this account." id)
        :errorp t))
      (t
       (tool-result (encode-json-to-string (report-json report)))))))

