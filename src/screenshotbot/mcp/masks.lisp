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
                #:masks
                #:set-channel-screenshot-mask)
  (:import-from #:screenshotbot/model/company
                #:find-channel)
  (:import-from #:screenshotbot/model/image
                #:mask-rect
                #:mask-rect-height
                #:mask-rect-left
                #:mask-rect-top
                #:mask-rect-width)
  (:documentation "The list_masks and edit_masks MCP tools.

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

;; ----------------------------------------------------------------------
;; Editing
;; ----------------------------------------------------------------------

(defparameter +max-masks-per-screenshot+ 100
  "Ceiling on how many rectangles one screenshot may be given at once.

The mask editor has no such limit because a person is dragging them one
at a time. A model generating them is a different proposition, and
nothing downstream bounds the list.")

(define-condition invalid-masks (error)
  ((message :initarg :message
            :reader invalid-masks-message))
  (:documentation "The masks argument could not be read.

A condition rather than a return value because the parse happens several
frames down, and threading a failure back up by hand is how half of them
end up ignored."))

(defun fail (format &rest args)
  (error 'invalid-masks :message (apply #'format nil format args)))

(defun decode-masks-argument (text)
  "Decode TEXT as JSON with member names left as written.

Locally rather than through the dispatcher's decoder because the reason
is local: `left' has to stay \"left\", and CL-JSON's default mapping would
turn it into a keyword whose name depends on settings this file does not
control."
  (handler-case
      (let ((json:*json-identifier-name-to-lisp* #'identity)
            (json:*identifier-name-to-key* #'identity))
        (json:decode-json-from-string text))
    (invalid-masks (e) (error e))
    (error ()
      (fail "masks is not valid JSON. Pass a JSON array like [{\"left\":0,\"top\":0,\"width\":100,\"height\":20}]."))))

(defun alist-p (x)
  "Is X a proper list of conses -- what CL-JSON gives for a JSON object?

Both halves matter. A bare object sent where an array was asked for
decodes to a list of dotted pairs, so each `element' is something like
("left" . 1), and EVERY and ASSOC both walk off the end of one. That is a
shape mistake on the caller's part, and it has to read as one rather than
as a type error escaping the tool."
  (and (consp x)
       (null (cdr (last x)))
       (every #'consp x)))

(defun rect-field (item key)
  (let ((cell (assoc key item :test #'equal)))
    (unless cell
      (fail "a mask is missing ~a" key))
    (let ((value (cdr cell)))
      (unless (integerp value)
        (fail "~a must be a whole number of pixels, got ~s" key value))
      value)))

(defun parse-rect (item)
  (unless (alist-p item)
    (fail "each mask must be an object with left, top, width and height"))
  (let ((left (rect-field item "left"))
        (top (rect-field item "top"))
        (width (rect-field item "width"))
        (height (rect-field item "height")))
    ;; Normalised on the way in, unlike the editor, which stores whichever
    ;; way the rectangle was dragged and leaves MASK-RECT-LEFT and friends
    ;; to sort it out on the way out. Both are read the same, but writing
    ;; the canonical form is what makes edit_masks then list_masks report
    ;; back the numbers that were sent.
    (make-instance 'mask-rect
                   :left (min left (+ left width))
                   :top (min top (+ top height))
                   :width (abs width)
                   :height (abs height))))

(defun parse-masks (text)
  "TEXT as a list of MASK-RECTs. Empty means no masks. Signals INVALID-MASKS."
  (if (str:emptyp text)
      nil
      (let ((decoded (decode-masks-argument text)))
        (cond
          ((null decoded)
           ;; "[]" and "{}" both decode to NIL, and both mean the same
           ;; thing here.
           nil)
          ((not (listp decoded))
           (fail "masks must be a JSON array of rectangles."))
          ((every #'alist-p decoded)
           (when (> (length decoded) +max-masks-per-screenshot+)
             (fail "~a masks is more than the ~a allowed on one screenshot."
                   (length decoded) +max-masks-per-screenshot+))
           (mapcar #'parse-rect decoded))
          ((alist-p decoded)
           ;; A single rectangle sent bare. Worth its own message: the
           ;; generic one talks about what a mask should look like, which
           ;; is not the mistake, and leaves the caller re-reading a
           ;; rectangle that was already correct.
           (fail "masks must be a JSON array, even for a single rectangle -- wrap it in []."))
          (t
           (fail "masks must be a JSON array of rectangles."))))))

(def-tool "edit_masks"
    ((name "channel" "The channel (project) name, as returned by list_channels")
     (screenshot "screenshot" "The screenshot name, exactly as it appears in fetch_report or fetch_run")
     (rects "masks"
            "A JSON array of rectangles, each with whole-number `left`, `top`, `width` and `height` in pixels from the top-left. For example [{\"left\":0,\"top\":0,\"width\":320,\"height\":24}]. Pass an empty string to remove every mask from this screenshot."
            :allow-empty t))
    :scope "api:write"
    "Set the masks on one screenshot of a Screenshotbot channel (project). A mask is a rectangle Screenshotbot ignores when comparing runs, used for regions that change every time such as clocks and animations. This REPLACES every mask on that screenshot rather than adding to them, so call list_masks first if you mean to keep the existing ones. The screenshot name is not checked against any run, so a name that does not match one masks nothing. Returns the screenshot's masks as list_masks would report them."
  (let ((channel (find-channel (auth:current-company) name)))
    (cond
      ((null channel)
       (tool-result
        (format nil "No channel named ~a in this account." name)
        :errorp t))
      ((not (auth:can-viewer-edit (auth:viewer-context hunchentoot:*request*)
                                  channel))
       (tool-result
        (format nil "You do not have permission to change the settings for ~a." name)
        :errorp t))
      (t
       (handler-case
           (let ((parsed (parse-masks rects)))
             (set-channel-screenshot-mask channel screenshot parsed)
             (tool-result
              (encode-json-to-string
               (masked-screenshot-json (cons screenshot parsed)))))
         (invalid-masks (e)
           ;; The model wrote this argument and can rewrite it, so the
           ;; message says what was wrong rather than that something was.
           (tool-result (invalid-masks-message e) :errorp t)))))))
