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
                #:channel-name
                #:review-policy)
  (:import-from #:screenshotbot/model/report
                #:acceptable-reviewer
                #:acceptable-state
                #:report
                #:report-acceptable
                #:report-channel
                #:report-previous-run
                #:report-run
                #:report-title)
  (:import-from #:screenshotbot/model/review-policy
                #:can-review?)
  (:import-from #:screenshotbot/model/screenshot
                #:screenshot-image
                #:screenshot-name)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:util/misc
                #:?.)
  (:documentation "The fetch_report and accept_report MCP tools.

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

;; ----------------------------------------------------------------------
;; Reviewing
;; ----------------------------------------------------------------------

(defun reviewer-name (acceptable)
  "Something a person would recognise as the reviewer, or NIL.

Falls back to the email because a user need not have set a full name, and
`the reviewer is null' right after a call that recorded one reads as a
bug in the tool."
  (when-let ((user (acceptable-reviewer acceptable)))
    (let ((name (auth:user-full-name user)))
      (if (str:emptyp name)
          (auth:user-email user)
          name))))

(defun review-json (report)
  "REPORT's review state, as the dashboard would show it.

`none' rather than null for a report nobody has reviewed: CL-JSON encodes
NIL as null, and null answering \"has this been reviewed?\" reads as
\"unknown\" rather than \"not yet\". REVIEWER is left out entirely when
there is none, for the same reason -- an absent key is a question not
answered, where null looks like an answer."
  (let* ((acceptable (report-acceptable report))
         (result (obj "id" (util:oid report)
                      "url" (dashboard-url "report" (util:oid report))
                      "title" (report-title report)
                      "reviewState" (string-downcase
                                     (or (acceptable-state acceptable)
                                         "none")))))
    (when-let ((name (reviewer-name acceptable)))
      (setf (gethash "reviewer" result) name))
    result))

(defun reviewable-by-policy-p (report user)
  "Does the channel's review policy let USER review REPORT?

The dashboard asks this before it will draw the Accept button, but its
POST handler does not re-check it. Asked here because a channel set to
`disallow-author' means someone decided authors must not sign off on
their own screenshots, and a model acting for the author is still the
author."
  (let ((channel (report-channel report)))
    (or (null channel)
        (can-review? (review-policy channel) report user))))

(def-tool "accept_report"
    ((id "report_id" "The report id, as it appears in a report URL"))
    :scope "api:write"
    "Accept the screenshot changes in a Screenshotbot report, recording them as reviewed and approved. This is the same action as pressing Accept on the report page, and it is not confined to Screenshotbot: where the report came from a pull request, accepting it updates the commit status Screenshotbot posted there, which is often what a merge is waiting on. Look at the changes before accepting them -- fetch_report lists them and compare_images shows what moved -- and check with whoever asked if you have not. Accepting an already-accepted report changes nothing and is safe to retry. To find the report id for a pull request, read that PR's checks or commit statuses: Screenshotbot posts one per channel, and its details link is /report/<report_id>. A GitHub MCP server, or the gh CLI, is the usual way to reach those -- this server can see reports but has no view of your pull requests, so it cannot make that connection for you. Returns the report's review state after the change."
  (let ((report (find-report-by-id id)))
    (cond
      ((null report)
       (tool-result
        (format nil "No report ~a is visible to this account." id)
        :errorp t))
      ((null (report-acceptable report))
       ;; Reports made outside a promotion -- a manual run comparison,
       ;; say -- have nothing to accept. Worth saying plainly: the
       ;; alternative is a model retrying a call that will never work.
       (tool-result
        (format nil "Report ~a has no review attached, so there is nothing to accept. Only reports created by a promotion, such as the one Screenshotbot posts on a pull request, can be reviewed." id)
        :errorp t))
      ((not (auth:can-viewer-edit (auth:viewer-context hunchentoot:*request*)
                                  (report-acceptable report)))
       (tool-result
        (format nil "You do not have permission to review report ~a." id)
        :errorp t))
      ((not (reviewable-by-policy-p report (auth:current-user)))
       (tool-result
        (format nil "The review policy for ~a does not let authors review their own screenshots, and this report's run was authored by this account. Someone else has to accept it."
                (?. channel-name (report-channel report)))
        :errorp t))
      (t
       ;; :USER is not decoration -- the acceptable keeps a history item
       ;; per change, and that is the whole audit trail for a review
       ;; nobody clicked through themselves.
       (setf (acceptable-state (report-acceptable report)
                               :user (auth:current-user))
             :accepted)
       (tool-result (encode-json-to-string (review-json report)))))))

