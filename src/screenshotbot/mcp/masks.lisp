;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/masks
  (:use #:cl)
  (:import-from #:json
                #:encode-json-to-string)
  (:import-from #:screenshotbot/mcp/mcp
                #:capped
                #:def-tool
                #:obj
                #:tool-result
                #:visible-to-caller)
  (:import-from #:screenshotbot/model/channel
                #:channel
                #:channel-name
                #:masks)
  (:import-from #:screenshotbot/model/company
                #:find-channel)
  (:import-from #:screenshotbot/model/image
                #:mask-rect-height
                #:mask-rect-left
                #:mask-rect-top
                #:mask-rect-width)
  (:documentation "The list_masks MCP tool.

A mask is a rectangle on a named screenshot that Screenshotbot ignores
when comparing runs. They live on the channel, keyed by screenshot name,
so they are a property of the project rather than of any one run."))
(in-package :screenshotbot/mcp/masks)

(defparameter +max-masked-screenshots+ 200
  "Cap on how many screenshots one call reports masks for. Same reasoning
as +MAX-CHANNELS+.")

(defun mask-json (rect)
  "One rectangle, in the shape the mask editor uses.

Through MASK-RECT-TOP and friends rather than the raw slots: a rectangle
dragged up or left is stored with a negative width or height, and those
readers are where it gets normalised. Reading the slots directly would
report a rectangle nobody could draw."
  (obj "left" (mask-rect-left rect)
       "top" (mask-rect-top rect)
       "width" (mask-rect-width rect)
       "height" (mask-rect-height rect)))

(defun masked-screenshot-json (entry)
  (destructuring-bind (name . rects) entry
    (obj "screenshot" name
         "masks" (coerce (mapcar #'mask-json rects) 'vector))))

(defun masked-screenshots (channel)
  "CHANNEL's mask alist, entries that still carry a rectangle, by name.

Clearing a screenshot's masks leaves its entry behind with an empty list
rather than removing it, so without the filter this would report
screenshots that are not masked at all.

Sorted for the same reason VISIBLE-CHANNELS is: an alist that reorders
between calls makes a model think something changed."
  (sort (remove-if-not #'cdr (copy-alist (masks channel)))
        #'string<
        :key #'car))

(def-tool "list_masks"
    ((name "channel" "The channel (project) name, as returned by list_channels"))
    "List the masks configured for a Screenshotbot channel (project). A mask is a rectangle on a named screenshot that Screenshotbot ignores when comparing runs, used for regions that change every time such as timestamps or animations. Returns JSON: an array of objects with a `screenshot` name and its `masks`, each having `left`, `top`, `width` and `height` in pixels measured from the top-left corner. Screenshots with no masks are omitted."
  (let ((channel (visible-to-caller (find-channel (auth:current-company) name)
                                    'channel)))
    (cond
      ((null channel)
       (tool-result
        (format nil "No channel named ~a in this account." name)
        :errorp t))
      (t
       (multiple-value-bind (listed total)
           (capped (masked-screenshots channel) +max-masked-screenshots+
                   #'masked-screenshot-json)
         (tool-result
          (format nil "~a~@[~%~%~a~]"
                  (encode-json-to-string listed)
                  (when total
                    (format nil "Showing the first ~a of ~a masked screenshots."
                            +max-masked-screenshots+ total)))))))))
