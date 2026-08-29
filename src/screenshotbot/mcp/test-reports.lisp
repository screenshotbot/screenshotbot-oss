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
  (:import-from #:screenshotbot/mcp/test-util
                #:user)
  (:import-from #:screenshotbot/model/channel
                #:channel
                #:review-policy-name)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/model/report
                #:acceptable-history
                #:acceptable-reviewer
                #:acceptable-state
                #:base-acceptable
                #:report
                #:report-acceptable)
  (:import-from #:util/store/object-id
                #:oid)
  (:documentation "The fetch_report and accept_report tools."))
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

;; ----------------------------------------------------------------------
;; accept_report
;; ----------------------------------------------------------------------

(defun reviewable (company channel)
  "A report with an acceptable on it, as a promotion would have left it.

MAKE-CHANGED-REPORT deliberately does not attach one: most of the tools
never look at the review state, and a report without an acceptable is a
real thing -- a run comparison nobody opened a pull request for."
  (let* ((report (make-changed-report company channel))
         (acceptable (make-instance 'base-acceptable :report report)))
    (setf (report-acceptable report) acceptable)
    (values report acceptable)))

(defun accept-as (token id)
  (tool-text (call-tool-as token "accept_report" (list (cons "report_id" id)))))

;; TOKEN-WITH is an FLET inside the CALLER fixture, so it is spliced into
;; each test body and is not reachable from a top-level DEFUN. Hence the
;; repetition below rather than a WRITER helper.

(test accepting-a-report-records-the-review-and-reports-it-back
  (with-fixture caller ()
    (multiple-value-bind (report acceptable)
        (reviewable company (add-channel "web"))
      (multiple-value-bind (text result) (accept-as (token-with '("api:read" "api:write")) (oid report))
        (is-false (field result "isError"))
        (is (eql :accepted (acceptable-state acceptable)))
        (let ((json (decode text)))
          (is (equal (oid report) (field json "id")))
          (is (equal "accepted" (field json "reviewState")))
          (is-true (str:containsp "/report/" (field json "url"))))))))

(test the-review-is-attributed-to-the-account-behind-the-token
  "Nobody clicked Accept, so the history item is the only record of who
this was done for."
  (with-fixture caller ()
    (setf (auth:user-full-name user) "Ada Lovelace")
    (multiple-value-bind (report acceptable)
        (reviewable company (add-channel "web"))
      (multiple-value-bind (text)
          (accept-as (token-with '("api:read" "api:write")) (oid report))
        (is (eql user (acceptable-reviewer acceptable)))
        (is (equal 1 (length (acceptable-history acceptable))))
        (is (equal "Ada Lovelace" (field (decode text) "reviewer")))))))

(test a-nameless-reviewer-falls-back-to-their-email
  "Reviewer null on the very call that recorded one reads as a broken tool."
  (with-fixture caller ()
    (setf (auth:user-email user) "ada@example.com")
    (multiple-value-bind (report) (reviewable company (add-channel "web"))
      (multiple-value-bind (text)
          (accept-as (token-with '("api:read" "api:write")) (oid report))
        (is (equal "ada@example.com" (field (decode text) "reviewer")))))))

(test accepting-twice-is-not-an-error
  "A model that loses the response and retries should not be told it broke
something."
  (with-fixture caller ()
    (multiple-value-bind (report acceptable)
        (reviewable company (add-channel "web"))
      (accept-as (token-with '("api:read" "api:write")) (oid report))
      (multiple-value-bind (text result) (accept-as (token-with '("api:read" "api:write")) (oid report))
        (is-false (field result "isError"))
        (is (equal "accepted" (field (decode text) "reviewState")))
        (is (eql :accepted (acceptable-state acceptable)))))))

(test accepting-a-report-that-was-rejected-flips-it
  (with-fixture caller ()
    (multiple-value-bind (report acceptable)
        (reviewable company (add-channel "web"))
      (setf (acceptable-state acceptable) :rejected)
      (accept-as (token-with '("api:read" "api:write")) (oid report))
      (is (eql :accepted (acceptable-state acceptable))))))

(test accepting-needs-the-write-scope
  "The endpoint only asks for api:read, whose consent line promises the
client will read. Signing off on a pull request is not reading."
  (with-fixture caller ()
    (multiple-value-bind (report acceptable)
        (reviewable company (add-channel "web"))
      (multiple-value-bind (text result) (accept-as token (oid report))
        (is-true (field result "isError"))
        (is-true (str:containsp "api:write" text))
        (is (equal nil (acceptable-state acceptable)))))))

(test a-report-with-no-acceptable-says-so-rather-than-silently-doing-nothing
  (with-fixture caller ()
    (let ((report (make-changed-report company (add-channel "web"))))
      (multiple-value-bind (text result) (accept-as (token-with '("api:read" "api:write")) (oid report))
        (is-true (field result "isError"))
        (is-true (str:containsp "nothing to accept" text))))))

(test a-guest-may-read-a-report-but-may-not-accept-it
  (with-fixture caller ()
    (multiple-value-bind (report acceptable)
        (reviewable company (add-channel "web"))
      (roles:ensure-has-role company user 'roles:guest)
      (is-false (field (nth-value 1 (fetch-report-as token (oid report)))
                       "isError"))
      (multiple-value-bind (text result) (accept-as (token-with '("api:read" "api:write")) (oid report))
        (is-true (field result "isError"))
        (is-true (str:containsp "permission" text))
        (is (equal nil (acceptable-state acceptable)))))))

(test another-accounts-report-cannot-be-accepted
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (channel (make-instance 'channel :name "theirs" :company other)))
      (multiple-value-bind (report acceptable) (reviewable other channel)
        (multiple-value-bind (text result) (accept-as (token-with '("api:read" "api:write")) (oid report))
          (declare (ignore text))
          (is-true (field result "isError"))
          (is (equal nil (acceptable-state acceptable))))))))

(test a-disallow-author-channel-refuses-the-author-their-own-review
  "The dashboard hides the Accept button in this case but its POST handler
does not re-check. A tool has no button to hide, so it has to ask."
  (with-fixture caller ()
    (setf (auth:user-email user) "someone@example.com")
    (let ((channel (add-channel "web")))
      (setf (review-policy-name channel) :disallow-author)
      (let* ((previous (make-recorder-run :company company :channel channel))
             (run (make-recorder-run :company company :channel channel
                                     :author "someone@example.com"))
             (report (make-instance 'report :run run :previous-run previous
                                            :channel channel :title "1 change"))
             (acceptable (make-instance 'base-acceptable :report report)))
        (setf (report-acceptable report) acceptable)
        (multiple-value-bind (text result) (accept-as (token-with '("api:read" "api:write")) (oid report))
          (is-true (field result "isError"))
          (is-true (str:containsp "authors review their own" text))
          (is (equal nil (acceptable-state acceptable))))
        ;; The same channel accepts a run someone else authored, so the
        ;; refusal is the policy talking and not the tool being broken.
        (let* ((run (make-recorder-run :company company :channel channel
                                       :author "colleague@example.com"))
               (report (make-instance 'report :run run
                                              :previous-run previous
                                              :channel channel :title "1 change"))
               (acceptable (make-instance 'base-acceptable :report report)))
          (setf (report-acceptable report) acceptable)
          (accept-as (token-with '("api:read" "api:write")) (oid report))
          (is (eql :accepted (acceptable-state acceptable))))))))
