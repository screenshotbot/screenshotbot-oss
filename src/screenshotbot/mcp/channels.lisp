;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/channels
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:json
                #:encode-json-to-string)
  (:import-from #:screenshotbot/mcp/mcp
                #:def-tool
                #:obj
                #:tool-result)
  (:import-from #:screenshotbot/model/channel
                #:channel-name)
  (:import-from #:screenshotbot/model/company
                #:company-channels)
  (:documentation "The list_channels MCP tool."))
(in-package :screenshotbot/mcp/channels)

(defparameter +max-channels+ 200
  "Cap on how many channels one call returns. A company with thousands of
them would otherwise produce a result no model can use and no reviewer
would enjoy reading in a log.")

(defun channel-url (channel)
  (format nil "~a/channels/~a"
          (string-right-trim "/" (installation-domain *installation*))
          (store-object-id channel)))

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
                                     "url" (channel-url channel)))
                              shown)
                      'vector))
             ;; Say so rather than silently truncating: a model that
             ;; cannot see the cut will confidently report a partial list
             ;; as the whole one.
             (when (> (length channels) +max-channels+)
               (format nil "Showing the first ~a of ~a channels."
                       +max-channels+ (length channels)))))))

(def-tool "list_channels" ()
    "List the channels (projects) in the authenticated Screenshotbot account. Returns JSON: an array of objects with `name` and `url`."
  (let ((company (auth:current-company)))
    (cond
      ((null company)
       (tool-result "This token is not associated with an account."
                    :errorp t))
      (t
       (list-channels-result company)))))

