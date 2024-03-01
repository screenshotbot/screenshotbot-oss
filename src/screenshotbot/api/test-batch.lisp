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
                #:batch-commit
                #:find-or-create-batch)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/api/batch
                #:make-batch-from-dto
                #:batch-to-dto)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-company)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-commit)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/test-batch)


(util/fiveam:def-suite)

(defun make-hash (str)
  (replace
   (copy-seq "0000000000000000000000000000000000000000")
   str))

(def-fixture state ()
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (batch (find-or-create-batch
                   :company company
                   :name "foo"
                   :repo "https://github.com/tdrhq/fast-example"
                   :commit (make-hash "abcd"))))
      (&body))))

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
