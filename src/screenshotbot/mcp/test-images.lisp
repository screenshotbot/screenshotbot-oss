;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/test-images
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/mcp/test-util
                #:add-channel
                #:call-tool-as
                #:caller
                #:company
                #:decode
                #:field
                #:make-changed-report
                #:static-asset
                #:token
                #:tool-text)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:util/store/object-id
                #:oid)
  (:documentation "The fetch_image_url tool."))
(in-package :screenshotbot/mcp/test-images)

(util/fiveam:def-suite)

(defun fetch-image-url-as (token id)
  (tool-text (call-tool-as token "fetch_image_url" (list (cons "image_id" id)))))

(defun an-image (company)
  (make-image :company company
              :pathname (static-asset "assets/images/example-view.svg.png")))

(test an-image-id-resolves-to-an-absolute-url
  "IMAGE-PUBLIC-URL can return a site-relative path, which is useless to a
model on the other side of the internet."
  (with-fixture caller ()
    (let ((image (an-image company)))
      (multiple-value-bind (text result) (fetch-image-url-as token (oid image))
        (is-false (field result "isError"))
        (let* ((json (decode text))
               (url (field json "url")))
          (is (equal (oid image) (field json "id")))
          (is-true url)
          (is-true (str:starts-with-p "http" url)))))))

(test the-ids-a-report-hands-out-are-the-ids-this-tool-accepts
  "The two tools are only useful composed, and nothing else checks that
one's output is the other's input. This is the test that caught
fetch_image_url looking images up with the wrong finder, where every id a
report handed out resolved to nothing."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (multiple-value-bind (report) (make-changed-report company channel)
        (let* ((report-json
                 (decode
                  (tool-text (call-tool-as token "fetch_report"
                                           (list (cons "report_id" (oid report)))))))
               (change (first (field report-json "changed"))))
          (dolist (side (list "before" "after"))
            (let ((image-id (field (field change side) "imageId")))
              (is-true image-id "~a had no imageId" side)
              (multiple-value-bind (text result) (fetch-image-url-as token image-id)
                (is-false (field result "isError")
                          "~a image id ~a did not resolve" side image-id)
                (is-true (str:containsp "http" text))))))))))

(test an-unknown-or-malformed-image-id-is-a-tool-error
  (with-fixture caller ()
    (dolist (id (list "not-an-oid" "" "000000000000000000000000"))
      (multiple-value-bind (text result) (fetch-image-url-as token id)
        (declare (ignore text))
        (is-true (field result "isError")
                 "id ~s did not produce a tool error" id)))))

(test an-image-belonging-to-another-account-is-not-resolvable
  "Otherwise an image id leaked from anywhere would resolve to a URL."
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (image (an-image other)))
      (multiple-value-bind (text result) (fetch-image-url-as token (oid image))
        (declare (ignore text))
        (is-true (field result "isError"))))))
