;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/mcp/test-batches
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
  (:import-from #:screenshotbot/model/batch
                #:batch-item
                #:find-or-create-batch)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:util/store/object-id
                #:oid)
  (:documentation "The list_batch_reports tool."))
(in-package :screenshotbot/mcp/test-batches)

(util/fiveam:def-suite)

(defun list-batch-as (token id)
  (tool-text (call-tool-as token "list_batch_reports"
                           (list (cons "batch_id" id)))))

(defun make-batch (company &key (commit "0123456789abcdef")
                             (name "screenshots")
                             (repo "https://github.com/o/r")
                             pull-request-url)
  "A batch as the API creates one.

Through FIND-OR-CREATE-BATCH rather than MAKE-INSTANCE because that is
the only way one is ever made: it is what fills the slots that have no
initform, and it is what registers the batch in the by-commit index."
  (find-or-create-batch :company company
                        :commit commit
                        :name name
                        :repo repo
                        :pull-request-url pull-request-url
                        :phabricator-diff-id nil))

(defun add-item (batch channel &key status run report (title ""))
  (make-instance 'batch-item
                 :batch batch
                 :channel channel
                 :status status
                 :run run
                 :report report
                 :title title))

(defun channels-named (json)
  (map 'list (lambda (item) (field item "channel")) (field json "channels")))

(defun channel-named (json name)
  (find name (field json "channels")
        :key (lambda (item) (field item "channel"))
        :test #'equal))

(test listing-an-unknown-batch-is-a-tool-error-not-a-crash
  "A model hands us whatever id it has -- often one it read off a check's
details URL. A malformed one has to come back as something it can read."
  (with-fixture caller ()
    (dolist (id (list "not-an-oid" "000000000000000000000000"))
      (multiple-value-bind (text result) (list-batch-as token id)
        (declare (ignore text))
        (is-true (field result "isError")
                 "id ~s did not produce a tool error" id)))))

(test an-unknown-and-a-forbidden-batch-are-indistinguishable
  "Otherwise a caller could enumerate which batch ids exist by watching
the error change."
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (batch (make-batch other)))
      (multiple-value-bind (theirs) (list-batch-as token (oid batch))
        (multiple-value-bind (missing)
            (list-batch-as token "000000000000000000000000")
          ;; Same shape of answer, differing only in the id echoed back.
          (is (equal (str:replace-all (oid batch) "ID" theirs)
                     (str:replace-all "000000000000000000000000" "ID" missing))))))))

(test listing-a-batch-without-the-scope-never-reaches-it
  (with-fixture caller ()
    (multiple-value-bind (body status)
        (call-tool-as (token-with '("profile")) "list_batch_reports"
                      '(("batch_id" . "x")))
      (declare (ignore body))
      (is (equal 403 status)))))

(test a-batch-lists-a-report-id-per-changed-channel
  "The point of the tool: a model that found the batch on a pull request
comes away with ids it can hand to fetch_report or accept_report."
  (with-fixture caller ()
    (let* ((web (add-channel "web"))
           (ios (add-channel "ios"))
           (web-report (make-changed-report company web))
           (ios-report (make-changed-report company ios))
           (batch (make-batch company
                              :pull-request-url "https://github.com/o/r/pull/12")))
      (add-item batch web :status :action-required :report web-report
                          :title "1 change")
      (add-item batch ios :status :action-required :report ios-report
                          :title "1 change")
      (multiple-value-bind (text result) (list-batch-as token (oid batch))
        (is-false (field result "isError"))
        (let ((json (decode text)))
          (is (equal (oid batch) (field json "id")))
          (is (equal "0123456789abcdef" (field json "commit")))
          (is (equal "screenshots" (field json "name")))
          (is (equal "https://github.com/o/r/pull/12"
                     (field json "pullRequestUrl")))
          (is-true (str:containsp "/batch/" (field json "url")))
          (is (equal 2 (length (field json "channels"))))
          (let ((web-item (channel-named json "web")))
            (is (equal "action-required" (field web-item "status")))
            (is (equal "1 change" (field web-item "title")))
            (is (equal (oid web-report) (field web-item "reportId")))
            (is-true (str:containsp "/report/" (field web-item "reportUrl"))))
          (is (equal (oid ios-report)
                     (field (channel-named json "ios") "reportId"))))))))

(test a-channel-with-nothing-to-review-is-listed-without-a-report-id
  "Null there would be an id a model could pass on, and it would pass it
on. The row stays, because `ios ran and did not change' is an answer."
  (with-fixture caller ()
    (let* ((run (make-recorder-run :company company :channel (add-channel "ios")))
           (batch (make-batch company)))
      (add-item batch (screenshotbot/model/recorder-run:recorder-run-channel run)
                :status :success :run run :title "No screenshots changed")
      (multiple-value-bind (text result) (list-batch-as token (oid batch))
        (is-false (field result "isError"))
        (let ((item (first (field (decode text) "channels"))))
          (is (equal "ios" (field item "channel")))
          (is (equal "success" (field item "status")))
          (is (equal (oid run) (field item "runId")))
          ;; Absent, not null. FIELD cannot tell the two apart, so this
          ;; asks for the pair itself.
          (is (equal nil (assoc "reportId" item :test #'equal)))
          (is (equal nil (assoc "reportUrl" item :test #'equal))))))))

(test an-empty-batch-lists-no-channels-rather-than-failing
  "A batch is created before the first run reports into it, so an empty
one is an ordinary state -- and a model handles [] where it handles a tool
failure by giving up."
  (with-fixture caller ()
    (let ((batch (make-batch company)))
      (multiple-value-bind (text result) (list-batch-as token (oid batch))
        (is-false (field result "isError"))
        ;; The empty array, not null: CL-JSON renders an empty list as
        ;; null, and null answers a different question.
        (is-true (str:containsp "\"channels\":[]" text))))))

(test channels-come-back-in-the-order-the-dashboard-shows-them
  "Worst first. A model that reads only the top of a long batch should see
what a person scrolling the page sees first."
  (with-fixture caller ()
    (let ((batch (make-batch company)))
      (add-item batch (add-channel "b-success") :status :success)
      (add-item batch (add-channel "a-pending") :status :pending)
      (add-item batch (add-channel "z-rejected") :status :rejected)
      (add-item batch (add-channel "m-action") :status :action-required)
      (multiple-value-bind (text) (list-batch-as token (oid batch))
        (is (equal '("z-rejected" "m-action" "a-pending" "b-success")
                   (channels-named (decode text))))))))

(test a-batch-belonging-to-another-account-is-not-readable
  (with-fixture caller ()
    (let* ((other (make-instance 'screenshotbot/model/company:company
                                 :name "someone else"))
           (channel (make-instance 'channel :name "theirs" :company other))
           (report (make-changed-report other channel))
           (batch (make-batch other)))
      (add-item batch channel :status :action-required :report report)
      (multiple-value-bind (text result) (list-batch-as token (oid batch))
        (is-true (field result "isError"))
        ;; And nothing about what was in it leaks into the refusal.
        (is-false (str:containsp (oid report) text))))))
