;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/images
  (:use #:cl)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:import-from #:json
                #:encode-json-to-string)
  (:import-from #:screenshotbot/mcp/mcp
                #:def-tool
                #:obj
                #:tool-result
                #:visible-to-caller)
  (:import-from #:screenshotbot/model/image
                #:find-image-by-oid
                #:image
                #:image-public-url)
  (:import-from #:screenshotbot/model/image-comparison
                #:find-image-comparison-on-images
                #:identical-p
                #:image-comparison-difference-value
                #:image-comparison-result)
  (:documentation "The fetch_image_url and compare_images MCP tools."))
(in-package :screenshotbot/mcp/images)

(defun find-image-by-id (id)
  "The image with ID, if this caller may see it.

Images are not in the generic object-id index -- they carry their own oid
and their own lookup -- so this cannot go through FIND-BY-OID, which
simply returns NIL for every image id."
  (visible-to-caller (ignore-errors (find-image-by-oid id)) 'image))

(defun image-url (image)
  "A publicly fetchable URL for IMAGE.

IMAGE-PUBLIC-URL can return a site-relative path, which is useless to a
model on the other side of the internet. Binding *CDN-DOMAIN* the way the
run API does makes MAKE-CDN absolutize it."
  (let ((util.cdn:*cdn-domain* (or util.cdn:*cdn-domain*
                                   (installation-domain *installation*))))
    (util.cdn:make-cdn (image-public-url image :originalp t))))

(def-tool "fetch_image_url"
    ((id "image_id" "An image id, as returned by fetch_report"))
    "Resolve a Screenshotbot image id into a URL. Image ids come from fetch_report. Returns JSON with a `url` you can fetch or view."
  (let ((image (find-image-by-id id)))
    (cond
      ((null image)
       ;; One answer for missing and forbidden, as with reports.
       (tool-result
        (format nil "No image ~a is visible to this account." id)
        :errorp t))
      (t
       (tool-result
        (encode-json-to-string
         (obj "id" (util:oid image)
              "url" (image-url image))))))))


(defun cached-comparison (before after)
  "The stored comparison of BEFORE and AFTER, or NIL if there is not one.

ONLY-CACHED-P is the whole point: computing a comparison shells out to
ImageMagick over two full-size images, which is far too slow to do inside
a tool call, and a model asking about a hundred screenshots would queue a
hundred of them. Reports that actually rendered a diff have theirs
already, which is the case worth being fast.

Order does not matter -- FIND-IMAGE-COMPARISON-ON-IMAGES sorts the pair by
store id before looking up -- so a model that has before and after the
wrong way round still gets the answer rather than a miss.

The company check inside it cannot fire here, since both images had to be
visible to this caller to get this far, but a comparison that signals is
still not something a tool call should die on."
  (ignore-errors
   (find-image-comparison-on-images before after :only-cached-p t)))

(def-tool "compare_images"
    ((before-id "before_image_id" "The image id from the `before` side of a change, as returned by fetch_report")
     (after-id "after_image_id" "The image id from the `after` side of the same change"))
    "Get a URL for the visual diff between two Screenshotbot images. The diff image is transparent everywhere the two images agree, and solid red on the pixels that differ, so it shows at a glance where and how much changed. Only returns an already-computed comparison and never starts one: reports that rendered a diff have theirs cached, so this is fast for exactly the changes fetch_report describes. When there is no cached comparison the result is `status: \"not-computed\"` rather than an error, which says nothing either way about whether the images differ -- fetch_image_url both of them and look."
  (let ((before (find-image-by-id before-id))
        (after (find-image-by-id after-id)))
    (cond
      ((null before)
       (tool-result
        (format nil "No image ~a is visible to this account." before-id)
        :errorp t))
      ((null after)
       (tool-result
        (format nil "No image ~a is visible to this account." after-id)
        :errorp t))
      (t
       (let ((comparison (cached-comparison before after)))
         (cond
           ((null comparison)
            (tool-result
             (format nil "~a~%~%~a"
                     (encode-json-to-string
                      ;; A status string rather than a boolean: CL-JSON
                      ;; cannot emit false -- NIL encodes as null, which a
                      ;; model reads as "unknown" rather than "no".
                      (obj "beforeImageId" (util:oid before)
                           "afterImageId" (util:oid after)
                           "status" "not-computed"))
                     "No comparison for these two images has been computed yet, and this tool does not start one. This does not mean the images are the same -- fetch_image_url both of them to compare them yourself.")))
           (t
            (let ((result
                    (obj "beforeImageId" (util:oid before)
                         "afterImageId" (util:oid after)
                         "status" "cached"
                         "url" (image-url (image-comparison-result comparison))
                         ;; "yes"/"no" for the same reason as STATUS above:
                         ;; NIL would encode as null, and "is this image
                         ;; identical?" answered with null reads as "we do
                         ;; not know" rather than "no".
                         ;;
                         ;; IDENTICAL-P is set when the two differ only in
                         ;; EXIF, which to anyone looking at them is the
                         ;; same picture -- hence the name here.
                         "pixelsIdentical" (if (identical-p comparison) "yes" "no"))))
              ;; Only when it was computed. It is NIL on comparisons made
              ;; before the value was recorded, and the backfill for that
              ;; deliberately does not run on this path.
              (when (image-comparison-difference-value comparison)
                (setf (gethash "differenceValue" result)
                      (image-comparison-difference-value comparison)))
              (tool-result (encode-json-to-string result))))))))))
