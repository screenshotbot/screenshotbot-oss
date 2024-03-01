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
                #:find-batches-for-commit
                #:batch-item
                #:find-batch-item
                #:find-or-create-batch)
  (:import-from #:screenshotbot/user-api
                #:can-view
                #:user)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains))
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
        (find-or-create-batch
         :company company
         :repo "http://foo.git"
         :commit "abcd"
         :name "foobar")
        (find-or-create-batch
         :company company
         :repo "http://foo.git"
         :commit "abcd"
         :name "foobar")))))

(test find-batch-item
  (with-fixture state ()
    (let ((batch (find-or-create-batch
                  :company company
                  :repo "http://foo.git"
                  :commit "abcd"
                  :name "foobar")))
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
    (let ((batch (find-or-create-batch
                  :company company
                  :repo "http://foo.git"
                  :commit "abcd"
                  :name "foobar")))
      (is-false (can-view batch user)))))

(test find-batches-for-company ()
  (with-fixture state ()
    (let ((other-company (make-instance 'company)))
      (let ((batch-1 (find-or-create-batch
                      :repo "https://foo.git"
                      :company company
                      :commit "abcd"
                      :name "foobar"))
            (batch-2 (find-or-create-batch
                      :repo "https://foo.git"
                      :company company
                      :commit "abcd"
                      :name "foobar-another"))
            (batch-3 (find-or-create-batch
                      :repo "https://foo.git"
                      :company other-company
                      :commit "abcd"))
            (batch-4 (find-or-create-batch
                      :repo "https://foo.git"
                      :company company
                      :commit "a000")))
        (is (not (eql batch-2 batch-1)))
        (assert-that (find-batches-for-commit :commit "abcd"
                                              :company company)
                     (contains
                      batch-1 batch-2))))))
