;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :scheduled-jobs
  (:use #:cl)
  (:import-from #:scheduled-jobs/model
                #:next-scheduled-job
                #:tz-offset
                #:cronexpr
                #:job
                #:%at
                #:*lock*
                #:at
                #:periodic
                #:scheduled-job-args
                #:scheduled-job-function
                #:scheduled-job)
  (:import-from #:bknr.indices
                #:object-destroyed-p)
  (:import-from #:bknr.datastore
                #:deftransaction
                #:*store*
                #:delete-object)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:scheduled-jobs/bindings
                #:cron-next
                #:cron-parse-expr)
  #+ (and lispworks linux)
  (:import-from #:bknr.cluster/server
                #:leaderp)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:make-scheduled-job
   #:now
   #:at
   #:bad-argument-error
   #:call-job
   #:*scheduled-job*))
(in-package :scheduled-jobs)


(defclass threaded-executor ()
  ())

(defvar *executor* (make-instance 'threaded-executor))

(defvar *scheduled-job* nil
  "The current scheduled job, bound when a job is being executed")

(defun now ()
  (get-universal-time))

(define-condition bad-argument-error (error)
  ((argument :initarg :argument)))

(defun is-valid-arg (arg)
  "A sanity check on the argument, to make sure the argument is
serializable. This does not gurrantee serializability though, since
we're not looking in to what the object references."
  (or
   (null arg)
   (stringp arg)
   (symbolp arg)
   (arrayp arg)
   (numberp arg)
   (listp arg)
   (typep arg 'bknr.datastore:store-object)))

(defmethod (setf cronexpr) ((val string) (self scheduled-job))
  (let ((parsed (cron-parse-expr val)))
    (with-transaction ()
      (setf (%at self) (cron-next parsed :now (now)
                                         :timezone (tz-offset self))))))

(defun make-scheduled-job (&key at
                             cronexpr
                             tzname
                             (function (error "must provide :function"))
                             (args (error "must provide :args"))
                             periodic)
  (unless (or at cronexpr)
    (error "must provide at least one of :at or :cronexpr"))
  (when (and at cronexpr)
    (error "Don't provide both :at and :cronexpr"))

  (when cronexpr
    (let ((parsed (cron-parse-expr cronexpr)))
      (setf at (cron-next parsed :now (now)
                          :timezone (tz-offset tzname)))))

  (loop for arg in args
        unless (is-valid-arg arg)
          do (error 'bad-argument-error :argument arg))
  (make-instance 'scheduled-job
                  :at at
                  :cronexpr cronexpr
                  :tzname tzname
                  :function function
                  :args args
                  :periodic periodic))


(defvar *call-pending-scheduled-jobs-lock* (bt:make-lock "call-pending-scheduled-job"))

(defun %call-pending-scheduled-jobs ()
  (let ((now (now)))
    (when (bt:with-lock-held (*lock*)
            (let ((next (next-scheduled-job)))
              (and
               next
               (< (at next) now))))
     (multiple-value-bind (next)
         (bt:with-lock-held (*lock*)
           (next-scheduled-job))
       (cond
         ((null next)
          :no-more)
         ((> (at next) now)
          (error "We should not be here, we should've handled this earlier"))
         (t
          (unwind-protect
               (call-job *executor*
                         (lambda (&rest args)
                           (let ((*scheduled-job* next))
                             (apply (scheduled-job-function next) args)))
                         (scheduled-job-args next))
            (%reschedule-job next now))
          (%call-pending-scheduled-jobs)))))))

(deftransaction update-at (next at)
  "Transaction to update both the AT slot, and the queue at the same
time. TODO: delete this."
  (setf (%at next) at))

(defun %reschedule-job (next now)
  "After a job has just been run, call this to reschedule the job onto
the queue. Internal detail."
  (flet ((schedule-at (at)
           (update-at next at)))
    (cond
      ((cronexpr next)
       ;; Figure out the next run time based on the cronexpr
       (let ((cron-expr (ignore-errors
                         (cron-parse-expr (cronexpr next)))))
         (when cron-expr
           (schedule-at (cron-next cron-expr :now (now)
                                             :timezone (tz-offset next))))))
      ((periodic next)
       (schedule-at (+ (periodic next) now)))
      (t
       (delete-object next)))))

(defun call-pending-scheduled-jobs ()
  (bt:with-lock-held (*call-pending-scheduled-jobs-lock*)
    (%call-pending-scheduled-jobs)))

(defmethod call-job ((executor t) fn args)
  (apply fn args))

(defmethod call-job ((executor threaded-executor) fn args)
  (util/threading:make-thread
   (lambda ()
     ;; I don't know if call-next-method will work reliably across
     ;; implementations. But it's not really needed for this.
     (call-job nil fn args))
   :name "schedule-job"))

(def-cron call-pending-scheduled-job ()
  (when (and
         (boundp 'bknr.datastore:*store*)
         #+ (and lispworks linux)
         (leaderp bknr.datastore:*store*))
   (call-pending-scheduled-jobs)))
