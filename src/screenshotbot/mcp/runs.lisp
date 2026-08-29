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
  (:import-from #:screenshotbot/model/recorder-run
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
  (:documentation "The fetch_run and fetch_active_run MCP tools.

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
