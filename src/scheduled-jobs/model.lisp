;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :scheduled-jobs/model
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:initialize-transient-instance)
  (:import-from #:priority-queue
                #:make-pqueue
                #:pqueue-push)
  (:import-from #:bknr.datastore
                #:*store*)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:scheduled-job
   #:scheduled-job-function
   #:scheduled-job-args
   #:priority-queue
   #:%at
   #:%update-queue
   #:job
   #:cronexpr))
(in-package :scheduled-jobs/model)

(defvar *lock* (bt:make-recursive-lock))

(defvar *priority-queues*
  (apply #'make-hash-table
           #+lispworks
           '(:weak-kind :key)
           #-lispworks
           '()))

(defun priority-queue ()
  (util:or-setf
   (gethash *store* *priority-queues*)
   (make-pqueue #'<)
   :thread-safe t))

(defclass scheduled-job (store-object)
  ((at :initarg :at
       :reader at
       :writer (setf %at) #| For internal use only |#)
   (function
    :initarg :function
    :reader scheduled-job-function)
   (args
    :initarg :args
    :reader scheduled-job-args)
   (cronexpr
    :initarg :cronexpr
    :initform nil
    :reader cronexpr
    :writer (setf %cronexpr))
   (tzname
    :initarg :tzname
    :initform nil
    :reader scheduled-job-tzname
    :documentation "When used with cronexpr, this determins the
    timezone in which to calculate the cron job.")
   (periodic
    :initarg :periodic
    :initform nil
    :reader periodic)
   (wrapper :initform nil
            :accessor wrapper
            :transient t))
  (:metaclass persistent-class))

(defclass wrapper ()
  ((job :initarg :job
        :reader job)))

(defun %update-queue (self)
  "Update the queue for the given object. This is exported for
internal use only."
  (let ((wrapper (make-instance 'wrapper :job self)))
    (setf (wrapper self) wrapper)
    (pqueue-push wrapper
                 (at self)
                 (priority-queue))))

(defmethod bknr.datastore:initialize-transient-instance ((self scheduled-job))
  (bt:with-lock-held (*lock*)
    (%update-queue self)))

(defmethod tz-offset ((self scheduled-job))
  (let ((tzname (scheduled-job-tzname self)))
    (cond
      (tzname
       (tz-offset tzname))
      (t 0))))

(defmethod tz-offset ((tzname string))
  (let ((tz (local-time:find-timezone-by-location-name tzname)))
    (cond
      (tz
       (/ (local-time:timestamp-subtimezone
           (local-time:now)
           tz)
          3600))
      (t
       (warn "Could not find timezone ~a" tzname)
       0))))

(defmethod tz-offset ((location null))
  0)
