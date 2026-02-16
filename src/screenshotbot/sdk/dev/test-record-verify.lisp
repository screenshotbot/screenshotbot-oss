;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev/test-record-verify
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/dev/record-verify
                #:record-run
                #:homedir
                #:%make-run-and-get-id
                #:record/command)
  (:import-from #:cl-mock
                #:if-called)
  (:import-from #:screenshotbot/sdk/sdk
                #:put-run
                #:request)
  (:import-from #:screenshotbot/api/model
                #:*api-version*)
  (:import-from #:screenshotbot/sdk/api-context
                #:remote-version)
  (:import-from #:screenshotbot/sdk/integration-fixture
                #:with-sdk-integration)
  (:import-from #:screenshotbot/model/recorder-run
                #:compare-threshold
                #:trunkp
                #:recorder-run)
  (:import-from #:fiveam-matchers/numbers
                #:is-number-close-to)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/dev/test-record-verify)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-sdk-integration (api-context)
   (cl-mock:with-mocks ()
     (if-called 'uiop:quit (lambda (code)
                             (error "exiting with: ~a" code)))
     (tmpdir:with-tmpdir (%homedir)
       (if-called 'homedir (lambda ()
                             %homedir))
       (tmpdir:with-tmpdir (dir)
         (uiop:copy-file
          #.(asdf:system-relative-pathname
             :screenshotbot.sdk
             "fixture/dir1/rose.png")
          (path:catfile dir "foo.png"))
         (&body))))))

(test record-happy-path
  (with-fixture state ()
    (finishes
      (record-run
       (%make-run-and-get-id
        api-context
        :directory dir
        :channel "foo")
       "foo"))
    (is (path:-e (path:catfile %homedir ".config/screenshotbot/recordings/foo.json")))))

(test run-has-productionp-as-nil
  "This is a critical test, because we don't want these to pollute the production runs"
  (with-fixture state ()
    (finishes
      (record-run
       (%make-run-and-get-id
        api-context
        :directory dir
        :channel "foo")
       "foo"))
    (let ((run (first (bknr.datastore:class-instances 'recorder-run))))
      (is-false (trunkp run)))))

(test run-has-theshold
  "This is a critical test, because we don't want these to pollute the production runs"
  (with-fixture state ()
    (finishes
      (record-run
       (%make-run-and-get-id
        api-context
        :compare-threshold 0.77
        :directory dir
        :channel "foo")
       "foo"))
    (let ((run (first (bknr.datastore:class-instances 'recorder-run))))
      (assert-that
       (compare-threshold run)
       (is-number-close-to 0.77)))))

(test 0-threshold-is-always-zero
  "This is a critical test, because we don't want these to pollute the production runs"
  (with-fixture state ()
    (finishes
      (record-run
       (%make-run-and-get-id
        api-context
        :compare-threshold 0.0
        :directory dir
        :channel "foo")
       "foo"))
    (let ((run (first (bknr.datastore:class-instances 'recorder-run))))
      (is (eql 0.0D0
               (compare-threshold run))))))


