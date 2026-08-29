;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/test-reports
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
                #:token
                #:token-with
                #:tool-text)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:util/store/object-id
                #:oid)
  (:documentation "The fetch_report tool."))
(in-package :screenshotbot/mcp/test-reports)

(util/fiveam:def-suite)

(defun fetch-report-as (token id)
  (tool-text (call-tool-as token "fetch_report" (list (cons "report_id" id)))))

(test fetching-an-unknown-report-is-a-tool-error-not-a-crash
  "A model hands us whatever id it has. A malformed one has to come back
as something it can read and correct."
  (with-fixture caller ()
    (dolist (id (list "not-an-oid" "" "000000000000000000000000"))
      (multiple-value-bind (text result) (fetch-report-as token id)
        (declare (ignore text))
        (is-true (field result "isError")
                 "id ~s did not produce a tool error" id)))))

(test an-unknown-and-a-forbidden-report-are-indistinguishable
  "Otherwise a caller could enumerate which report ids exist by watching
the error change."
  (with-fixture caller ()
    (multiple-value-bind (missing) (fetch-report-as token "000000000000000000000000")
      (multiple-value-bind (malformed) (fetch-report-as token "111111111111111111111111")
        ;; Same shape of answer, differing only in the id echoed back.
        (is (equal (str:replace-all "000000000000000000000000" "ID" missing)
                   (str:replace-all "111111111111111111111111" "ID" malformed)))))))

(test fetching-a-report-without-the-scope-never-reaches-it
  (with-fixture caller ()
    (multiple-value-bind (body status)
        (call-tool-as (token-with '("profile")) "fetch_report"
                      '(("report_id" . "x")))
      (declare (ignore body))
      (is (equal 403 status)))))

(test a-report-describes-what-changed-with-image-ids
  "The point of the tool: enough for a model to ask for the two images and
see the difference itself."
  (with-fixture caller ()
    (let ((channel (add-channel "web")))
      (multiple-value-bind (report before-image after-image)
          (make-changed-report company channel)
        (multiple-value-bind (text result)
            (fetch-report-as token (oid report))
          (is-false (field result "isError"))
          (let* ((json (decode text))
                 (changed (field json "changed")))
            (is (equal (oid report) (field json "id")))
            (is (equal "1 change" (field json "title")))
            (is (equal "web" (field json "channel")))
            (is-true (str:containsp "/report/" (field json "url")))
            (is (equal 1 (length changed)))
            (is (equal "home" (field (first changed) "name")))
            ;; The ids a model then feeds to fetch_image_url, and they are
            ;; the right way round -- before is the previous run's.
            (is (equal (oid before-image)
                       (field (field (first changed) "before") "imageId")))
            (is (equal (oid after-image)
                       (field (field (first changed) "after") "imageId")))))))))

(test a-report-belonging-to-another-account-is-not-readable
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (channel (make-instance 'channel :name "theirs" :company other)))
      (multiple-value-bind (report) (make-changed-report other channel)
        (multiple-value-bind (text result) (fetch-report-as token (oid report))
          (declare (ignore text))
          (is-true (field result "isError")))))))
