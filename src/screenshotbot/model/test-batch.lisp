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
                #:delete-old-unchanged-batch-items
                #:batch-items
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
                #:contains)
  (:import-from #:screenshotbot/model/recorder-run
                #:promotion-log)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:auth/viewer-context
                #:normal-viewer-context)
  (:import-from #:bknr.indices
                #:object-destroyed-p
                #:should-index-objects-for-class-p))
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
      (is-false (auth:can-viewer-view
                 (make-instance 'normal-viewer-context
                                :user user)
                 batch)))))

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


(test promotion-log-for-batch
  (with-fixture state ()
    (let ((batch (find-or-create-batch
                  :repo "dfdf"
                  :company company
                  :commit "abcd")))
      (is (pathnamep
           (blob-pathname
            (promotion-log batch)))))))


(test saving-batch-items
  (tmpdir:with-tmpdir (dir)
    (with-test-store (:dir dir)
      (make-instance 'batch-item :batch "foo"
                                 :title "bar")
      (finishes (bknr.datastore:snapshot)))
    (with-test-store (:dir dir)
      (pass))))

(test batch-items-are-not-in-the-class-index
  (is-false (should-index-objects-for-class-p (find-class 'batch-item))))

(test class-instances-still-finds-batch-items
  (with-fixture state ()
    (let* ((batch (find-or-create-batch
                   :company company
                   :repo "http://foo.git"
                   :commit "abcd"
                   :name "foobar"))
           (item (make-instance 'batch-item
                                :batch batch
                                :channel channel)))
      (assert-that (bknr.datastore:class-instances 'batch-item)
                   (contains item)))))


(def-fixture deletion ()
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (channel (find-or-create-channel company "test-channel"))
           (batch (find-or-create-batch
                   :company company
                   :repo "http://foo.git"
                   :commit "abcd"
                   :name "foobar"))
           (unchanged (make-instance 'batch-item
                                     :batch batch
                                     :channel channel
                                     :status :success))
           (changed (make-instance 'batch-item
                                   :batch batch
                                   :channel :other
                                   :status :action-required))
           (in-100-days (+ (get-universal-time) (* 100 24 3600))))
      (&body))))

(test delete-old-unchanged-batch-items
  (with-fixture deletion ()
    (is (eql 1 (delete-old-unchanged-batch-items :now in-100-days)))
    (is (object-destroyed-p unchanged))
    (is-false (object-destroyed-p changed))
    (assert-that (fset:convert 'list (batch-items batch))
                 (contains changed))))

(test recent-batch-items-are-not-deleted
  (with-fixture deletion ()
    (is (eql 0 (delete-old-unchanged-batch-items)))
    (is-false (object-destroyed-p unchanged))))

(test deletes-items-from-several-batches
  (with-fixture deletion ()
    (let ((more (loop for i below 5
                      collect (make-instance
                               'batch-item
                               :batch (find-or-create-batch
                                       :company company
                                       :repo "http://foo.git"
                                       :commit (format nil "commit-~a" i)
                                       :name "foobar")
                               :status :success))))
      (is (eql 6 (delete-old-unchanged-batch-items :now in-100-days)))
      (is (every #'object-destroyed-p more)))))

(test deletes-batches-with-more-items-than-call-arguments-limit
  "LispWorks' CALL-ARGUMENTS-LIMIT is 2047. (SBCL's is huge, so don't
use the constant here.)"
  (with-fixture deletion ()
    (loop repeat 2100
          do (make-instance 'batch-item
                            :batch batch
                            :status :success))
    (is (eql 2101
             (delete-old-unchanged-batch-items :now in-100-days)))))
