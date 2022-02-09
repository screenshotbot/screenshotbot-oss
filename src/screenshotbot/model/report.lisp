;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/report
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/core
        #:screenshotbot/report-api
        #:screenshotbot/model/view)
  (:import-from #:screenshotbot/user-api
                #:report-num-changes)
  (:import-from #:util #:object-with-oid)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-channel
                #:publicp)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object
                #:with-transaction)
  (:import-from #:bknr.indices
                #:hash-index)
  (:export
   #:report
   #:report-run
   #:report-title
   #:base-acceptable
   #:acceptable-state
   #:report-previous-run
   #:report-channel
   #:report-acceptable
   #:github-task
   #:report-num-changes
   #:reports-for-run))
(in-package :screenshotbot/model/report)

(defclass report (object-with-oid)
  ((run
    :initarg :run
    :accessor report-run
    :index-type hash-index
    :index-reader reports-for-run)
   (title
    :type string
    :initarg :title
    :accessor report-title)
   (previous-run
    :initarg :previous-run
    :accessor report-previous-run)
   (channel
    :initarg :channel
    :initform nil
    :accessor report-channel)
   (acceptable
    :initarg :acceptable
    :initform nil
    :accessor report-acceptable)
   (github-task
    :initform nil
    :initarg :github-task
    :accessor github-task)
   (num-changes ;; see report-num-changes
    :initform 0
    :initarg :num-changes
    :accessor report-num-changes)
   (created-at
    :accessor %created-at))
  (:metaclass has-created-at))

(defmethod can-view ((report report) user)
  (and
   (can-view (report-run report) user)
   (can-view (report-previous-run report) user)))

(defmethod can-public-view ((report report))
  ;; The public can view any report associated with a public github
  ;; repo
  (publicp (recorder-run-channel (report-run report))))

(defclass base-acceptable (store-object)
  ((state :initform nil
          :initarg :state
          :documentation "One of NIL, :ACCEPTED, :REJECTED"
          :reader acceptable-state))
  (:metaclass persistent-class))

(defmethod (setf acceptable-state) (state (acceptable base-acceptable))
  (assert (member state (list nil :accepted :rejected)))
  (with-transaction ()
    (setf (slot-value acceptable 'state)
          state)))
