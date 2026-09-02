;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/batches
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:json
                #:encode-json-to-string)
  (:import-from #:screenshotbot/dashboard/batch
                #:sort-items)
  (:import-from #:screenshotbot/mcp/mcp
                #:capped
                #:dashboard-url
                #:def-tool
                #:obj
                #:tool-result
                #:visible-to-caller)
  (:import-from #:screenshotbot/model/recorder-run
                #:pull-request-url
                #:recorder-run)
  (:import-from #:screenshotbot/model/report
                #:report)
  (:import-from #:screenshotbot/user-api
                #:channel-name)
  (:import-from #:util/misc
                #:?.)
  (:local-nicknames (#:batch #:screenshotbot/model/batch))
  (:documentation "The list_batch_reports MCP tool.

A batch is one commit's worth of runs across every channel, grouped so
that a pull request gets a single check instead of one per channel. It is
the only thing in the model that knows a pull request produced *these*
reports, which makes it the way from a PR to the ids the report tools
take."))
(in-package :screenshotbot/mcp/batches)

(defparameter +max-items+ 200
  "Cap on channels reported for one batch. Same reasoning as
+MAX-CHANNELS+: an account batching a thousand channels onto one commit
produces a result no model can use.")

(defun find-batch-by-id (id)
  "The batch with ID, if this caller may see it.

Never signals, for the same reason FIND-REPORT-BY-ID does not: a model
hands us whatever string it has, and a malformed id has to come back as
something it can read and correct rather than as an internal error it can
only retry."
  (visible-to-caller (ignore-errors (util:find-by-oid id 'batch:batch))
                     'batch:batch))

(defun item-json (item)
  "One channel's line in the batch.

REPORTID is omitted rather than null when the channel produced no report,
which is the ordinary outcome when nothing changed. Null there reads as
an id a model can pass on, and it would pass it on.

Both ids go through VISIBLE-TO-CALLER even though the batch as a whole
already passed. It is the same belt and braces VISIBLE-CHANNELS uses:
listing objects without asking is the habit that eventually lists the
wrong ones."
  (let ((result
          (obj "channel" (?. channel-name (batch:batch-item-channel item))
               ;; The status the dashboard colours the row by, and the one
               ;; rolled up into the pull request's check. `action-required'
               ;; is the one that means a person still has to look.
               "status" (string-downcase
                         (or (batch:batch-item-status item) "unknown"))
               "title" (batch:batch-item-title item))))
    (when-let ((report (visible-to-caller (batch:batch-item-report item) 'report)))
      (setf (gethash "reportId" result) (util:oid report))
      (setf (gethash "reportUrl" result) (dashboard-url "report" (util:oid report))))
    (when-let ((run (visible-to-caller (batch:batch-item-run item) 'recorder-run)))
      (setf (gethash "runId" result) (util:oid run)))
    result))

(defun batch-items-in-dashboard-order (batch)
  "BATCH's items, worst status first, then by channel name.

The dashboard's own ordering, reused rather than reinvented: a model that
reads only the first few lines of a long batch should see the same things
a person scrolling the page sees first."
  (bt:with-lock-held ((batch:lock batch))
    (sort-items (fset:convert 'list (batch:batch-items batch)))))

(defun batch-json (batch)
  (multiple-value-bind (items total)
      (capped (batch-items-in-dashboard-order batch) +max-items+ #'item-json)
    (let ((result
            (obj "id" (util:oid batch)
                 "url" (dashboard-url "batch" (util:oid batch))
                 "name" (batch:batch-name batch)
                 "commit" (batch:batch-commit batch)
                 "pullRequestUrl" (pull-request-url batch)
                 "channels" items)))
      ;; Stated, not implied by a short list. A model that cannot see the
      ;; cut reports a partial batch as the whole one, and here that means
      ;; telling someone a pull request is clear when it is not.
      (when total
        (setf (gethash "channelsTotal" result) total))
      result)))

(def-tool "list_batch_reports"
    ((id "batch_id" "The batch id, as it appears in a batch URL"))
    "List every channel in a Screenshotbot batch, with the report id for each one that has changes. A batch is one commit's worth of runs across all channels, grouped so a pull request gets a single check instead of one per channel. Use this to go from a pull request to the reports it produced: pass each `reportId` to fetch_report to see what changed, or to accept_report to sign it off.

To find the batch id for a pull request, read that PR's checks or commit statuses -- Screenshotbot posts one for the batch and its details link is /batch/<batch_id>. A GitHub MCP server, or the gh CLI, is the usual way to reach those; this server can see batches but has no view of your pull requests, so it cannot make that connection for you. If the details link is /report/<report_id> instead, that account is not using batches and the check already points at the single report.

Returns JSON with the batch's commit and pull request URL, and a `channels` array ordered the way the dashboard orders it -- the ones needing attention first. A channel whose screenshots did not change has no `reportId`; its `status` says what happened. `action-required` is the status that means a person still has to review."
  (let ((batch (find-batch-by-id id)))
    (cond
      ((null batch)
       ;; Deliberately one message for "no such batch" and "not yours", as
       ;; with reports and runs: distinguishing them would let a caller
       ;; enumerate which batch ids exist.
       (tool-result
        (format nil "No batch ~a is visible to this account." id)
        :errorp t))
      (t
       (tool-result (encode-json-to-string (batch-json batch)))))))
