;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :scheduled-jobs/test-scheduled-jobs
  (:use #:cl
        #:fiveam
        #:scheduled-jobs)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:scheduled-jobs
                #:*executor*
                #:call-pending-scheduled-jobs
                #:now
                #:scheduled-job)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:bknr.indices
                #:object-destroyed-p)
  (:import-from #:scheduled-jobs/model
                #:tz-offset
                #:cronexpr
                #:%update-queue
                #:%at
                #:at)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:scheduled-jobs/bindings
                #:*unix-epoch-difference*)
  (:local-nicknames (#:a #:alexandria)))
(in-package :scheduled-jobs/test-scheduled-jobs)

(util/fiveam:def-suite)

(defvar *state* nil)

(def-fixture state ()
  (with-test-store ()
    (cl-mock:with-mocks ()
      (let ((time 0)
            (*executor* nil))
        (cl-mock:if-called 'now
                            (lambda () time))
        (unwind-protect
             (&body)
          (setf *state* nil))))))

(test make-model
  (with-fixture state ()
    (finishes
      (make-instance 'scheduled-job :at 0))))


(test make-scheduled-job
  (with-fixture state ()
    (finishes
      (make-scheduled-job :at 20
                          :function 'foo
                          :args nil))))

(test make-model-with-cron
  (with-fixture state ()
    (setf time (unix 0))
    (let ((job (make-scheduled-job :cronexpr "0 0 * * * *"
                                   :function 'foo
                                   :args nil)))
      (is (eql (at job) (unix 3600))))))

(defclass zoidberg () ())

(test check-args-for-store-objects
  (with-fixture state ()
    (signals bad-argument-error
      (make-scheduled-job :at 20
                          :function 'foo
                          :args (list
                                 (make-instance 'zoidberg))))))

(defun foo ()
  (setf *state* :done))

(test sanity-check
  (with-fixture state ()
    (finishes
     (make-scheduled-job :at 0
                         :function 'foo
                         :args '()))
    (setf time 1)
    (call-pending-scheduled-jobs)
    (is (eql :done *state*))))

(defun unix (x)
  (+ x *unix-epoch-difference*
     ;; allow for negative timezones etc
     (* 3600 24 10)))


(test always-looking-at-the-time
  (with-fixture state ()
    (setf time 0)
    (finishes
     (make-scheduled-job :at 20
                         :function 'foo
                         :args '()))
    (setf time 10)
    (call-pending-scheduled-jobs)
    (is (eql nil *state*))
    (setf time 30)
    (call-pending-scheduled-jobs)
    (is (eql :done *state*))))

(test always-looking-at-the-time-for-cronexpr
  (with-fixture state ()
    (setf time (unix 0))
    (finishes
     (make-scheduled-job :cronexpr "20 * * * * *"
                         :function 'foo
                         :args '()))
    (setf time (unix 10))
    (call-pending-scheduled-jobs)
    (is (eql nil *state*))
    (setf time (unix 30))
    (call-pending-scheduled-jobs)
    (is (eql :done *state*))))

(test dont-run-deleted-job
  (with-fixture state ()
    (let ((job (make-scheduled-job :at 20
                                   :function 'foo
                                   :args '())))
      (setf time 30)
      (delete-object job)
      (call-pending-scheduled-jobs)
      (is (eql nil *state*)))))

(test dont-run-deleted-job-for-cronexpr
  (with-fixture state ()
    (setf time (unix 0))
    (let ((job (make-scheduled-job :cronexpr "20 * * * * *"
                                   :function 'foo
                                   :args '())))
      (setf time (unix 30))
      (delete-object job)
      (call-pending-scheduled-jobs)
      (is (eql nil *state*)))))

(test old-objects-are-immediately-deleted
  (with-fixture state ()
    (let ((job (make-scheduled-job :at 20
                                   :function 'foo
                                   :args '())))
      (setf time 30)
      (call-pending-scheduled-jobs)
      (is (object-destroyed-p job)))))

(test periodic-job-is-rescheduled
  (with-fixture state ()
    (let ((job (make-scheduled-job :at 20
                                   :function 'foo
                                   :periodic 15
                                   :args '())))
      (setf time 30)
      (call-pending-scheduled-jobs)
      (is (eql :done *state*))
      (is (eql 45 (at job)))
      (setf *state* nil)
      (setf time 40)
      (call-pending-scheduled-jobs)
      (is (eql nil *state*))
      (setf time 50)
      (call-pending-scheduled-jobs)
      (is (eql :done *state*)))))

(test periodic-job-is-rescheduled-for-cronexpr
  (with-fixture state ()
    (setf time (unix 0))
    (let ((job (make-scheduled-job :cronexpr "20,35,50 * * * * *"
                                   :function 'foo
                                   :args '())))
      ;; Note that for cron jobs, it isn't scheduled immediately after
      ;; the previous job is done, instead it's always scheduled on
      ;; the clock.
      (setf time (unix 30))
      (call-pending-scheduled-jobs)
      (is (eql :done *state*))
      (is (eql (unix 35) (at job)))
      (setf *state* nil)
      (setf time (unix 33))
      (call-pending-scheduled-jobs)
      (is (eql nil *state*))
      (setf time (unix 50))
      (call-pending-scheduled-jobs)
      (is (eql :done *state*)))))

(test if-AT-is-changed-we-update-queue
  (with-fixture state ()
    (let ((job (make-scheduled-job :at 20
                                   :function 'foo
                                   :args '())))
      (setf time 30)
      (with-transaction ()
        (setf (%at job) 40))
      (%update-queue job)
      (call-pending-scheduled-jobs)
      (is (eql nil *state*)))))

(test if-cronexpr-is-changed-we-update-queue
  (with-fixture state ()
    (setf time (unix 0))
    (let ((job (make-scheduled-job :cronexpr "20 * * * * *"
                                   :function 'foo
                                   :args '())))
      (setf time (unix 30))
      (setf (cronexpr job) "40 * * * * *")
      (%update-queue job)
      (call-pending-scheduled-jobs)
      (is (eql nil *state*)))))

(test tz-offset-is-correctly-computed
  (with-fixture state ()
    (setf time (unix 0))
    (let ((job (make-scheduled-job :cronexpr "20 * * * * *"
                                   :function 'foo
                                   :tzname "Asia/Calcutta"
                                   :args '())))
      (is (equal 11/2 (tz-offset job))))))

(defvar *ctr*)

(defun incr-ctr ()
  (incf *ctr*))

(test if-AT-is-changed-multiple-times-we-only-call-once-we-update-queue
  (with-fixture state ()
    (let ((*ctr* 0))
     (let ((job (make-scheduled-job :at 20
                                    :function 'incr-ctr
                                    :args '())))
       (with-transaction ()
         (setf (%at job) 20))
       (%update-queue job)
       (with-transaction ()
         (setf (%at job) 20))
       (%update-queue job)
       (setf time 30)
       (call-pending-scheduled-jobs)
       (is (eql 1 *ctr*))))))

(test if-AT-is-changed-multiple-times-we-only-call-once-we-update-queue-periodic
  (with-fixture state ()
    (let ((*ctr* 0))
     (let ((job (make-scheduled-job :at 20
                                    :function 'incr-ctr
                                    :periodic 100
                                    :args '())))
       (with-transaction ()
         (setf (%at job) 20))
       (%update-queue job)
       (with-transaction ()
         (setf (%at job) 20))
       (%update-queue job)
       (setf time 30)
       (call-pending-scheduled-jobs)
       (is (eql 1 *ctr*))))))

(test multiple-jobs-get-called
  (with-fixture state ()
    (let ((*ctr* 0))
      (make-scheduled-job :at 20
                          :function 'incr-ctr
                          :args '())
      (make-scheduled-job :at 19
                          :function 'incr-ctr
                          :args '())
      (setf time 21)
      (call-pending-scheduled-jobs)
      (is (eql 2 *ctr*)))))
