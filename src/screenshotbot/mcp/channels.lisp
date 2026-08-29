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
                #:company-channels)
  (:documentation "The list_channels MCP tool."))
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
              (coerce (mapcar (lambda (channel)
                                (obj "name" (channel-name channel)
                                     "url" (channel-url channel)
                                     "slackChannels" (slack-channels channel)))
                              shown)
                      'vector))
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

