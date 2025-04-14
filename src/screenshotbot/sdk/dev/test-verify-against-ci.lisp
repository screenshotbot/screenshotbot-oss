;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev/test-verify-against-ci
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/integration-fixture
                #:with-sdk-integration)
  (:import-from #:screenshotbot/model/company
                #:find-or-create-channel
                #:company)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:util/fake-clingon
                #:make-fake-clingon)
  (:import-from #:screenshotbot/sdk/dev/verify-against-ci
                #:parse-threshold
                #:find-base-run
                #:%verify-against-ci
                #:%options)
  (:import-from #:fiveam-matchers/errors
                #:signals-error-matching
                #:error-with-string-matching)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:import-from #:auto-restart
                #:*global-enable-auto-retries-p*)
  (:local-nicknames (#:run-context #:screenshotbot/sdk/run-context)
                    (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/sdk/dev/test-verify-against-ci)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((*global-enable-auto-retries-p* nil))
   (cl-mock:with-mocks ()
     (cl-mock:if-called 'uiop:quit (lambda (err)))
     (with-sdk-integration (api-context :company company)
       (let* ((channel (find-or-create-channel company "foobar"))
              (run (make-recorder-run
                    :company company
                    :commit-hash "abcd"
                    :channel channel
                    :trunkp t
                    :screenshots nil)))
         (&body))))))

(test find-base-run-test
  (with-fixture state ()
    (let ((run (find-base-run api-context "foobar" "abcd")))
      (assert-that run
                   (has-typep 'dto:run)))))

(test verify-against-ci
  (with-fixture state ()
    (let ((cmd (make-fake-clingon (%options)
                                  :directory (namestring
                                              (asdf:system-relative-pathname
                                               :screenshotbot.sdk
                                               "fixture/dir1/"))
                                  :image-file-types "png"
                                  :recursivep nil
                                  :channel "foobar"
                                  :base-commit-hash "abcd")))
      (%verify-against-ci api-context
                          cmd))))

(test verify-against-ci-when-base-commit-doesnt-exist
  (with-fixture state ()
    (let ((cmd (make-fake-clingon (%options)
                                  :directory (namestring
                                              (asdf:system-relative-pathname
                                               :screenshotbot.sdk
                                               "fixture/dir1/"))
                                  :image-file-types "png"
                                  :recursivep nil
                                  :channel "foobar"
                                  :base-commit-hash "dcba")))
      (signals-error-matching ()
          (%verify-against-ci api-context
                              cmd)
          (error-with-string-matching
           (matches-regex ".*Could not find run for commit.*"))))))

(test parse-threshold
  (eql 0.111 (parse-threshold "0.111"))
  (eql 1 (parse-threshold "1"))
  (eql nil (parse-threshold nil))
  (eql nil (parse-threshold ""))  
  (signals-error-matching ()
                          (parse-threshold "sdfdsfds")
                          (error-with-string-matching
                           (matches-regex ".*Threshold must be a floating point number between 0 and 1: got ")))
  (signals-error-matching ()
                          (parse-threshold "2.0")
                          (error-with-string-matching
                           (matches-regex ".*Threshold must be a floating point number between 0 and 1: got "))))
