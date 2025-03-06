;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/test-batch
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/batch
                #:batch-item
                #:batch-commit
                #:find-or-create-batch)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/api/batch
                #:get-reports
                #:make-batch-from-dto
                #:batch-to-dto)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run-company)
  (:import-from #:screenshotbot/user-api
                #:channel
                #:recorder-run-commit)
  (:import-from #:util/store/object-id
                #:oid)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/test-batch)


(util/fiveam:def-suite)

(defun make-hash (str)
  (replace
   (copy-seq "0000000000000000000000000000000000000000")
   str))

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:logged-in-p t
                     :company company)
     (let* ((batch (find-or-create-batch
                    :company company
                    :name "foo"
                    :repo "https://github.com/tdrhq/fast-example"
                    :commit (make-hash "abcd")))
            (channel (make-instance 'channel
                                    :company company))
            (run (make-recorder-run
                  :channel channel
                  :company company
                  :screenshots nil)))
       (&body)))))

(test batch-to-dto-happy-path
  (with-fixture state ()
    (let ((dto (batch-to-dto batch)))
      (is (equal (make-hash "abcd") (dto:batch-commit dto))))))

(test make-batch-from-dto
  (with-fixture state ()
    (let ((dto (make-instance 'dto:batch
                              :name "foo"
                              :github-repo "https://github.com/tdrhq/fast-example"
                              :commit (make-hash "abcdef"))))
      (let ((batch (make-batch-from-dto dto company)))
        (is (eql company (recorder-run-company batch)))
        (is (equal (make-hash "abcdef") (batch-commit batch)))))))

(test get-reports
  (with-fixture state ()
    (is (equalp #()  (get-reports :oid (oid batch))))
    (make-instance 'batch-item
                   :batch batch
                   :report (make-instance 'report
                                          :run run
                                          :previous-run nil))
    (assert-that (coerce (get-reports :oid (oid batch)) 'list)
                 (contains
                  (has-typep 'dto:report)))))

(test get-reports-when-some-reports-are-NIL
  (with-fixture state ()
    (is (equalp #()  (get-reports :oid (oid batch))))
    (make-instance 'batch-item
                   :batch batch
                   :report (make-instance 'report
                                          :run run
                                          :previous-run nil))
    (make-instance 'batch-item
                   :batch batch
                   :run run)
    (assert-that (coerce (get-reports :oid (oid batch)) 'list)
                 (contains
                  (has-typep 'dto:report)))))
