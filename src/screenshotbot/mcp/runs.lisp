;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/runs
  (:use #:cl)
  (:import-from #:json
                #:encode-json-to-string)
  (:import-from #:screenshotbot/mcp/mcp
                #:capped
                #:dashboard-url
                #:def-tool
                #:obj
                #:tool-result
                #:visible-to-caller)
  (:import-from #:screenshotbot/model/channel
                #:all-active-runs
                #:channel
                #:channel-name)
  (:import-from #:screenshotbot/model/company
                #:find-channel)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:screenshotbot/model/recorder-run
                #:promotion-log
                #:pull-request-url
                #:recorder-run
                #:recorder-run-author
                #:recorder-run-branch
                #:recorder-run-channel
                #:recorder-run-commit
                #:recorder-run-screenshots
                #:recorder-run-tags
                #:recorder-run-work-branch
                #:run-build-url)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot-image
                #:screenshot-name)
  (:documentation "The fetch_run, fetch_active_run and promotion_logs_for_run
MCP tools.

A run is a set of screenshots recorded from one CI job. fetch_report says
what *changed* between two of them; these say what a single run contains,
and which run a channel is currently comparing against."))
(in-package :screenshotbot/mcp/runs)

(defparameter +max-screenshots+ 200
  "Cap on screenshots listed for one run. Runs regularly carry more than a
model can use in one go, and unlike a report there is no diff narrowing
them down.")

(defun find-run-by-id (id)
  "The run with ID, if this caller may see it.

Never signals, for the same reason FIND-REPORT-BY-ID does not: a model
hands us whatever string it has, and a malformed id has to come back as
something it can read and correct."
  (visible-to-caller (ignore-errors (util:find-by-oid id 'recorder-run))
                     'recorder-run))

(defun screenshot-json (screenshot)
  (let ((image (screenshot-image screenshot)))
    (obj "name" (screenshot-name screenshot)
         ;; The id rather than the URL, as in fetch_report: a run can carry
         ;; hundreds of screenshots and a model should spend a call only on
         ;; the ones it decides to look at. fetch_image_url resolves them.
         "imageId" (when image (util:oid image)))))

(defun run-json (run &key screenshots)
  "RUN as JSON. Screenshots are left out unless asked for, because the
callers that list many runs want to identify them, not read them."
  (let ((result
          (obj "id" (util:oid run)
               "url" (dashboard-url "runs" (util:oid run))
               "channel" (let ((channel (recorder-run-channel run)))
                           (when channel (channel-name channel)))
               "commit" (recorder-run-commit run)
               ;; MAIN-BRANCH and WORK-BRANCH are different things and the
               ;; difference matters to anyone reasoning about a pull
               ;; request, so both are named for what they are. (The run
               ;; model's BRANCH slot is the main branch, despite the
               ;; name.)
               "mainBranch" (recorder-run-branch run)
               "workBranch" (recorder-run-work-branch run)
               "author" (recorder-run-author run)
               "pullRequestUrl" (pull-request-url run)
               "buildUrl" (run-build-url run)
               "tags" (coerce (recorder-run-tags run) 'vector))))
    (when screenshots
      (multiple-value-bind (listed total)
          (capped (recorder-run-screenshots run) +max-screenshots+
                  #'screenshot-json)
        (setf (gethash "screenshots" result) listed)
        (when total
          (setf (gethash "screenshotsTotal" result) total))))
    result))

(def-tool "fetch_run"
    ((id "run_id" "The run id, as it appears in a run URL"))
    "Fetch a Screenshotbot run by id. A run is the set of screenshots recorded by one CI job. Returns JSON with the run's commit, branches and build, plus each screenshot's name and image id. Use fetch_image_url to turn an image id into a URL you can look at."
  (let ((run (find-run-by-id id)))
    (cond
      ((null run)
       ;; One message for "no such run" and "not yours", as with reports:
       ;; distinguishing them would let a caller enumerate run ids.
       (tool-result
        (format nil "No run ~a is visible to this account." id)
        :errorp t))
      (t
       (tool-result (encode-json-to-string (run-json run :screenshots t)))))))

(defun active-runs (channel)
  "The active run per branch of CHANNEL, as (BRANCH . RUN), visible ones only.

The viewer check repeats what the channel lookup already established, for
the same reason VISIBLE-CHANNELS does it: listing objects without asking
is the habit that eventually lists the wrong ones."
  (let ((viewer (auth:viewer-context hunchentoot:*request*)))
    (loop for (branch . run) in (all-active-runs channel)
          if (auth:can-viewer-view viewer run)
            collect (cons branch run))))

(defun active-run-json (entry)
  (destructuring-bind (branch . run) entry
    ;; The branch is the one this run is active *for*, which is not
    ;; necessarily either branch recorded on the run itself.
    (obj "branch" branch
         "run" (run-json run))))

(def-tool "fetch_active_run"
    ((name "channel" "The channel (project) name, as returned by list_channels"))
    "Find the currently active runs for a Screenshotbot channel -- the runs new screenshots are compared against, one per branch. Returns JSON with an entry per branch. Screenshots are not included; call fetch_run with a run id for those."
  (let ((channel (visible-to-caller (find-channel (auth:current-company) name)
                                    'channel)))
    (cond
      ((null channel)
       (tool-result
        (format nil "No channel named ~a in this account." name)
        :errorp t))
      (t
       (tool-result
        (encode-json-to-string
         (obj "channel" (channel-name channel)
              ;; An empty array rather than an error: a channel that has
              ;; never promoted a run is a fact worth reporting, and a
              ;; model handles [] fine where it handles a tool failure by
              ;; giving up.
              "activeRuns" (coerce (mapcar #'active-run-json
                                           (active-runs channel))
                                   'vector))))))))

;; ----------------------------------------------------------------------
;; Promotion logs
;; ----------------------------------------------------------------------

(defparameter +max-log-characters+ 20000
  "How much of a promotion log one call returns.

Kept from the END rather than the start when it does not all fit: a
promotion that went wrong says so where it stopped, and the opening lines
are the same on every run.")

(defun promotion-log-text (run)
  "RUN's promotion log, or NIL if it was never written.

A missing file is ordinary. The log is written during promotion, so a run
that has not been promoted -- or one old enough to predate the current
storage layout -- simply has none."
  (let ((file (ignore-errors (blob-pathname (promotion-log run)))))
    (when (and file (path:-e file))
      (ignore-errors (uiop:read-file-string file)))))

(defun tail (text max)
  "The last MAX characters of TEXT. Second value is what was dropped."
  (if (<= (length text) max)
      (values text nil)
      (values (subseq text (- (length text) max))
              (- (length text) max))))

(def-tool "promotion_logs_for_run"
    ((id "run_id" "The run id, as it appears in a run URL or in fetch_report's `run` field"))
    "Read the promotion log for a Screenshotbot run. Promotion is the step where Screenshotbot decides which earlier run a new run is compared against, and produces a report if anything changed. This is a debugging tool for when a run produced no report, compared against the wrong thing, or seems stuck. Returns the log as plain text.

These logs are internal diagnostics written for Screenshotbot's own developers, not for users. Assume whoever is asking does not know what the lines mean, and do not simply quote them back: read them, work out what happened, and explain it in ordinary terms. Screenshotbot's server is open source at https://github.com/screenshotbot/screenshotbot-oss -- when a log line is unclear, look up the code that emits it there rather than guessing, and say when you are unsure."
  (let ((run (find-run-by-id id)))
    (cond
      ((null run)
       (tool-result
        (format nil "No run ~a is visible to this account." id)
        :errorp t))
      (t
       (let ((text (promotion-log-text run)))
         (cond
           ((null text)
            ;; Not an error: it is the ordinary state of a run that was
            ;; never promoted, and it is itself a useful thing to know
            ;; when someone is asking why no report appeared.
            (tool-result
             (format nil "Run ~a has no promotion log. That usually means promotion never ran for it -- which is itself worth explaining if the question was why no report appeared." (util:oid run))))
           (t
            (multiple-value-bind (shown dropped) (tail text +max-log-characters+)
              (tool-result
               (format nil "Promotion log for run ~a~@[ on channel ~a~]~@[, commit ~a~].~@[~%~%~a~]~%~%--BEGIN LOGS--~%~a~%--END LOGS--"
                       (util:oid run)
                       (let ((channel (recorder-run-channel run)))
                         (when channel (channel-name channel)))
                       (recorder-run-commit run)
                       (when dropped
                         (format nil "The first ~a characters are not shown; this is the end of the log, where a failure would be."
                                 dropped))
                       shown)))))))))) 
