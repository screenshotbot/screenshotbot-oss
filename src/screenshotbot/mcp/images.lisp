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
  (:documentation "The fetch_image_url MCP tool."))
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

