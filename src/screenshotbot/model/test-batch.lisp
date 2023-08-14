;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-batch
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:find-or-create-channel
                #:company)
  (:import-from #:screenshotbot/model/batch
                #:batch-item
                #:find-batch-item
                #:find-or-create-batch)
  (:import-from #:screenshotbot/user-api
                #:can-view
                #:user)
  (:import-from #:screenshotbot/testing
                #:with-installation))
(in-package :screenshotbot/model/test-batch)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-installation ()
     (let* ((company (make-instance 'company))
            (user (make-instance 'user))
            (channel (find-or-create-channel company "test-channel")))
       (&body)))))

(test find-instead-of-create
  (with-fixture state ()
   (is (eql
        (find-or-create-batch company "http://foo.git" "abcd")
        (find-or-create-batch company "http://foo.git" "abcd")))))

(test find-batch-item
  (with-fixture state ()
    (let ((batch (find-or-create-batch company "http://foo.git" "abcd")))
      (let ((case1 (make-instance 'batch-item
                     :batch batch
                     :channel :foo))
            (res (make-instance 'batch-item
                                :batch batch
                                :channel channel)))
        (is (eql res (find-batch-item batch :channel channel)))
        (is (eql case1 (find-batch-item batch :channel :foo)))))))


(test can-view
  (with-fixture state ()
    (let ((batch (find-or-create-batch company "http://foo.git" "abcd")))
      (is-false (can-view batch user)))))
