;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-batch-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/batch
                #:batch-items
                #:state-invalidated-p
                #:batch-item
                #:batch)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:check-summary
                #:check-title
                #:check-status
                #:push-remote-check
                #:push-remote-check-via-batching
                #:make-check
                #:abstract-pr-promoter)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/batch-promoter
                #:compute-check-if-invalidated
                #:compute-check
                #:compute-title
                #:compute-status)
  (:import-from #:screenshotbot/model/company
                #:company))
(in-package :screenshotbot/test-batch-promoter)

(util/fiveam:def-suite)

(defclass test-promoter (abstract-pr-promoter)
  ((checks :initform nil
           :accessor checks)))

(defmethod push-remote-check ((self test-promoter)
                              (batch batch)
                              check)
  (push check (checks self)))

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (let* ((company (make-instance 'company))
            (channel (make-instance 'channel
                                    :company company
                                    :name "channel-1"))
            (batch (make-instance 'batch
                                  :company company
                                  :commit "abcd"
                                  :name "my-batch"))
            (run (make-recorder-run
                  :channel channel
                  :batch batch))
            (promoter (make-instance 'test-promoter)))
       (&body)))))

(test simple-batching-test
  (with-fixture state ()
    (finishes
     (push-remote-check-via-batching
      promoter
      batch
      run
      (make-check run
                  :status :accepted
                  :title "bar"
                  :sha "foo")))
    (is (eql 1 (length (checks promoter))))))

(test compute-status-happy-path
  (with-fixture state ()
    (is (eql :action-required
             (compute-status
              (fset:convert 'fset:set
                            (list
                             (make-instance 'batch-item
                                            :status :success)
                             (make-instance 'batch-item
                                            :status :action-required))))))
    (is (eql :accepted
             (compute-status
              (fset:convert 'fset:set
                            (list
                             (make-instance 'batch-item
                                            :status :success)
                             (make-instance 'batch-item
                                            :status :accepted))))))
    (is (eql :rejected
             (compute-status
              (fset:convert 'fset:set
                            (list
                             (make-instance 'batch-item
                                            :status :rejected)
                             (make-instance 'batch-item
                                            :status :accepted))))))))

(test compute-title
  (with-fixture state ()
    (is (equal "some thing some thing"
               (compute-title
                (fset:convert 'fset:set
                              (list
                               (make-instance 'batch-item
                                              :title "some thing some thing"))))))))


(test compute-check-if-invalidated
  (with-fixture state ()
    (let* ((batch (make-instance 'batch :commit "abcd"
                                 :company company
                                        :name "FooBar"))
           (item (make-instance 'batch-item :batch batch
                                :run run
                                            :channel channel
                                            :status :rejected)))
      (setf (state-invalidated-p batch) t)
      (let ((check (compute-check-if-invalidated batch :user)))
        (is-true check)
        (is (eql :rejected (check-status check))))
      (is (eql nil (state-invalidated-p batch)))
      (is (null (compute-check-if-invalidated batch :user))))))

(test compute-status-for-empty-batch
  (with-fixture state ()
    (is (eql :success (compute-status (batch-items batch))))))

(test compute-check-for-empty-batch
  (with-fixture state ()
    (let ((check
            (compute-check batch :user :my-user)))
      (is (equal :success (check-status check)))
      (is (equal "No screenshots changed" (check-title check)))
      (is (equal "Nothing to review" (check-summary check))))))
