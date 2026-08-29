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
                #:token-with
                #:tool-text)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/model/image-comparison
                #:find-image-comparison-on-images)
  (:import-from #:util/store/object-id
                #:oid)
  (:documentation "The fetch_image_url tool."))
(in-package :screenshotbot/mcp/test-images)

(util/fiveam:def-suite)

(defun decode-first (text)
  "The JSON object at the head of TEXT.

COMPARE_IMAGES may follow its JSON with a note for the model, and
ENCODE-JSON-TO-STRING never puts a newline inside the JSON itself."
  (decode (first (str:lines text))))

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

;; ----------------------------------------------------------------------
;; compare_images
;; ----------------------------------------------------------------------

(defun compare-images-as (token before-id after-id)
  (tool-text (call-tool-as token "compare_images"
                           (list (cons "before_image_id" before-id)
                                 (cons "after_image_id" after-id)))))

(defun another-image (company)
  "An image that genuinely differs from AN-IMAGE, so a comparison of the
two has pixels in it."
  (make-image :company company
              :pathname (static-asset "assets/images/example-view-square.svg.png")))

(test an-uncomputed-comparison-is-reported-as-such-and-is-not-an-error
  "The tool never starts a comparison, so `no cached result' is an answer
rather than a failure -- and a model that read it as a failure would stop
rather than go and look at the two images itself."
  (with-fixture caller ()
    (let ((before (an-image company))
          (after (another-image company)))
      (multiple-value-bind (text result)
          (compare-images-as token (oid before) (oid after))
        (is-false (field result "isError"))
        (is (equal "not-computed" (field (decode-first text) "status")))
        ;; And it must not have quietly computed one on the way past.
        (is-false (find-image-comparison-on-images before after
                                                   :only-cached-p t))))))

(test the-uncomputed-answer-says-it-is-not-a-claim-that-they-match
  "The dangerous misreading. `status: not-computed' with nothing else
would invite a model to report the screenshots as unchanged."
  (with-fixture caller ()
    (let ((text (compare-images-as token
                                   (oid (an-image company))
                                   (oid (another-image company)))))
      (is-true (str:containsp "does not mean the images are the same" text)))))

(test a-cached-comparison-comes-back-with-a-url
  (with-fixture caller ()
    (let* ((before (an-image company))
           (after (another-image company)))
      ;; Compute one the way a report would have.
      (find-image-comparison-on-images before after)
      (multiple-value-bind (text result)
          (compare-images-as token (oid before) (oid after))
        (is-false (field result "isError"))
        (let ((json (decode-first text)))
          (is (equal "cached" (field json "status")))
          (is-true (str:starts-with-p "http" (field json "url")))
          (is (equal (oid before) (field json "beforeImageId")))
          (is (equal (oid after) (field json "afterImageId")))
          ;; A string, because CL-JSON cannot say false and a model
          ;; reading null would take it for "unknown".
          (is (equal "no" (field json "pixelsIdentical"))))))))

(test the-order-the-two-ids-arrive-in-does-not-matter
  "The store sorts the pair by id before looking up, so a model that has
before and after the wrong way round still gets its answer."
  (with-fixture caller ()
    (let ((before (an-image company))
          (after (another-image company)))
      (find-image-comparison-on-images before after)
      (is (equal "cached"
                 (field (decode-first (compare-images-as token (oid after) (oid before)))
                        "status"))))))

(test comparing-an-image-with-itself-reports-the-pixels-identical
  (with-fixture caller ()
    (let ((image (an-image company))
          (same (an-image company)))
      (find-image-comparison-on-images image same)
      (let ((json (decode-first (compare-images-as token (oid image) (oid same)))))
        (is (equal "cached" (field json "status")))
        (is (equal "yes" (field json "pixelsIdentical")))))))

(test an-unknown-image-id-on-either-side-is-a-tool-error
  (with-fixture caller ()
    (let ((known (oid (an-image company))))
      (dolist (pair (list (list "not-an-oid" known)
                          (list known "not-an-oid")))
        (multiple-value-bind (text result)
            (compare-images-as token (first pair) (second pair))
          (declare (ignore text))
          (is-true (field result "isError")
                   "~s did not produce a tool error" pair))))))

(test another-accounts-image-cannot-be-compared
  "Otherwise an image id leaked from anywhere would resolve to a diff."
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (mine (an-image company))
           (theirs (an-image other)))
      (multiple-value-bind (text result)
          (compare-images-as token (oid mine) (oid theirs))
        (declare (ignore text))
        (is-true (field result "isError"))))))

(test comparing-images-without-the-scope-never-reaches-it
  (with-fixture caller ()
    (let ((before (an-image company))
          (after (another-image company)))
      (find-image-comparison-on-images before after)
      (multiple-value-bind (body status)
          (call-tool-as (token-with '("profile")) "compare_images"
                        (list (cons "before_image_id" (oid before))
                              (cons "after_image_id" (oid after))))
        (declare (ignore body))
        (is (equal 403 status))))))
