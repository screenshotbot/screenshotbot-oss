;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/channels
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:json
                #:encode-json-to-string)
  (:import-from #:screenshotbot/mcp/mcp
                #:dashboard-url
                #:def-tool
                #:obj
                #:tool-result)
  (:import-from #:screenshotbot/model/channel
                #:channel-name
                #:channel-slack-channels)
  (:import-from #:screenshotbot/model/company
                #:company-channels
                #:find-channel)
  (:documentation "The list_channels and update_slack_channels MCP tools."))
(in-package :screenshotbot/mcp/channels)

(defparameter +max-channels+ 200
  "Cap on how many channels one call returns. A company with thousands of
them would otherwise produce a result no model can use and no reviewer
would enjoy reading in a log.")

(defun channel-url (channel)
  (dashboard-url "channels" (store-object-id channel)))

(defun slack-channels (channel)
  "The Slack channels notified when CHANNEL changes.

With the leading '#'. They are stored without one -- the settings page
strips it on save and SEND-TASK puts it back before posting -- but '#eng'
is what a person calls the channel, and a bare 'eng' invites a model to
report it as something else.

This is the per-channel setting only. An account may also have a default
Slack channel that receives everything, and there are tag rules keyed off
a run's tags; neither is a property of the channel."
  ;; A vector, so a channel notifying nobody renders as [] rather than the
  ;; null CL-JSON gives for an empty list.
  (coerce (mapcar (lambda (name)
                    (str:ensure-prefix "#" name))
                  (channel-slack-channels channel))
          'vector))

(defun channel-json (channel)
  (obj "name" (channel-name channel)
       "url" (channel-url channel)
       "slackChannels" (slack-channels channel)))

(defun visible-channels (company)
  "CHANNELS of COMPANY this caller may see, in a stable order.

The viewer-context check is belt and braces -- every channel here belongs
to the company the token authenticated as -- but listing objects without
asking is the habit that eventually lists the wrong ones."
  (let ((viewer (auth:viewer-context hunchentoot:*request*)))
    (sort
     (remove-if-not (lambda (channel)
                      (auth:can-viewer-view viewer channel))
                    (company-channels company))
     #'string<
     :key #'channel-name)))

(defun list-channels-result (company)
  (let* ((channels (visible-channels company))
         (shown (if (> (length channels) +max-channels+)
                    (subseq channels 0 +max-channels+)
                    channels)))
    (tool-result
     (format nil "~a~@[~%~%~a~]"
             (encode-json-to-string
              ;; A vector, so that no channels renders as [] rather than
              ;; the null CL-JSON gives for an empty list. A model reading
              ;; null has been told something quite different from "there
              ;; are none".
              (coerce (mapcar #'channel-json shown) 'vector))
             ;; Say so rather than silently truncating: a model that
             ;; cannot see the cut will confidently report a partial list
             ;; as the whole one.
             (when (> (length channels) +max-channels+)
               (format nil "Showing the first ~a of ~a channels."
                       +max-channels+ (length channels)))))))

(def-tool "list_channels" ()
    "List the channels (projects) in the authenticated Screenshotbot account. Returns JSON: an array of objects with `name`, `url`, and `slackChannels` -- the Slack channels notified when that channel changes, which is empty if none are configured."
  (let ((company (auth:current-company)))
    (cond
      ((null company)
       (tool-result "This token is not associated with an account."
                    :errorp t))
      (t
       (list-channels-result company)))))


(defparameter +max-slack-channels+ 100
  "Ceiling on how many Slack channels one Screenshotbot channel may
notify, exclusive. The dashboard asserts (< n 100) on the same slot, and
two paths writing one slot had better agree on what it will hold.")

(defun parse-slack-channels (text)
  "Parse a comma separated list of Slack channel names into stored form.

Stored without the '#', which is what the settings page writes and what
SEND-TASK expects to have to add back. The normalisation is deliberately
the dashboard's, down to stripping every '#' rather than only a leading
one, because a name typed into the settings box and the same name sent
here must end up as the same string -- otherwise which door you came
through starts to matter."
  (remove-if #'str:emptyp
             (mapcar #'str:trim
                     (mapcar (lambda (name)
                               (str:replace-all "#" "" name))
                             (str:split "," text)))))

(def-tool "update_slack_channels"
    ((name "channel" "The channel (project) name, as returned by list_channels")
     (slack "slack_channels"
            "Comma separated Slack channel names, for example \"#eng, #releases\". Pass an empty string to stop notifying anyone."
            :allow-empty t))
    :scope "api:write"
    "Set which Slack channels are notified when a Screenshotbot channel (project) changes. This REPLACES the channel's current list rather than adding to it, so call list_channels first if you mean to keep what is already there. Returns the channel's updated settings."
  (let ((channel (find-channel (auth:current-company) name)))
    (cond
      ((null channel)
       ;; Same wording as fetch_active_run's: lookup is scoped to the
       ;; caller's company, so a name that exists elsewhere is simply
       ;; absent, and saying anything more would confirm it exists.
       (tool-result
        (format nil "No channel named ~a in this account." name)
        :errorp t))
      ((not (auth:can-viewer-edit (auth:viewer-context hunchentoot:*request*)
                                  channel))
       ;; Reading a channel and reconfiguring it are different questions,
       ;; and a token that passed the first does not automatically pass
       ;; the second: a guest can list channels and may not change them.
       (tool-result
        (format nil "You do not have permission to change the settings for ~a." name)
        :errorp t))
      (t
       (let ((parsed (parse-slack-channels slack)))
         (cond
           ((>= (length parsed) +max-slack-channels+)
            (tool-result
             (format nil "~a Slack channels is too many; fewer than ~a are allowed."
                     (length parsed) +max-slack-channels+)
             :errorp t))
           (t
            (setf (channel-slack-channels channel) parsed)
            ;; The updated channel in the same shape list_channels uses,
            ;; so a model can see what it just did without another call.
            (tool-result (encode-json-to-string (channel-json channel))))))))))
