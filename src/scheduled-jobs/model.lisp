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
  (:import-from #:bknr.datastore
                #:*store*)
  (:import-from #:util/store/store
                #:defindex)
  (:import-from #:util/store/fset-index
                #:index-least
                #:fset-set-index)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:scheduled-job
   #:scheduled-job-function
   #:scheduled-job-args
   #:%at
   #:job
   #:cronexpr
   #:next-scheduled-job))
(in-package :scheduled-jobs/model)

(defvar *lock* (bt:make-recursive-lock))

;; TODO: replace this logic with def-store-local. It's best to time
;; this with a restart

(defindex +scheduled-job-index+
  'fset-set-index
  :slot-name 'at)

(defclass scheduled-job (store-object)
  ((at :initarg :at
       :reader at
       :index +scheduled-job-index+
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

(defun next-scheduled-job ()
  (index-least +scheduled-job-index+))


(defmethod bknr.datastore:initialize-transient-instance ((self scheduled-job))
  ;; TODO: delete
  (call-next-method))

(defmethod tz-offset ((self scheduled-job))
  (let ((tzname (scheduled-job-tzname self)))
    (cond
      (tzname
       (tz-offset tzname))
      (t 0))))

(defmethod tz-offset ((tzname string))
  (let* ((tzname (if (uiop:os-windows-p) (str:replace-all "/" "\\" tzname) tzname))
         (tz (local-time:find-timezone-by-location-name tzname)))
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
