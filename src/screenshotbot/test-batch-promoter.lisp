;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-batch-promoter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/batch
                #:batch)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/abstract-pr-promoter
                #:push-remote-check
                #:push-remote-check-via-batching
                #:make-check
                #:abstract-pr-promoter)
  (:import-from #:screenshotbot/user-api
                #:channel))
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
  (with-test-store ()
    (let* ((channel (make-instance 'channel
                                   :name "channel-1"))
           (batch (make-instance 'batch
                                 :name "my-batch"))
           (run (make-recorder-run
                 :channel channel
                 :batch batch))
           (promoter (make-instance 'test-promoter)))
      (&body))))

(test simple-batching-test
  (with-fixture state ()
    (finishes
     (push-remote-check-via-batching
      promoter
      batch
      run
      (make-check run
                  :sha "foo")))
    (is (eql 1 (length (checks promoter))))))
