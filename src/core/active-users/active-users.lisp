;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/active-users/active-users
  (:use #:cl)
  (:import-from #:util/store/store
                #:defindex
                #:with-class-validation)
  (:import-from #:util/store/fset-index
                #:fset-unique-index)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:bknr.indices
                #:index-get)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:screenshotbot/impersonation
                #:make-impersonation
                #:impersonatedp)
  (:export
   #:mark-active-user
   #:active-users-for-company))
(in-package :core/active-users/active-users)

(defvar *lock* (bt:make-lock))

(defindex +lookup-index+
  'fset-unique-index
  :slots '(%date %user %company))

(with-class-validation
  (defclass active-user (store-object)
    ((%user :initarg :user
            :reader user)
     (%company :initarg :company
               :reader company)
     (%date :initarg :date
            :reader active-user-date)
     (%ip-address :initform (make-hash-table :test #'equal)
                  :transient t
                  :accessor active-user-ip-addresses
                  :documentation "All the IP addresses this has been accessed from. This is a transient
slot and is not stored to disk, so we don't need to hash the IP
addresses."))
    (:class-indices (3d-coords :index +lookup-index+))
    (:metaclass persistent-class)))


(defun active-users-for-company (company &key (days-ago 60)
                                           (company-test #'eql))
  "COMPANY-TEST is called on the company of the active-user and the
company provided, in that order. In particular this is used to support
sub-companies, which this code can't be aware of."
  (let ((start-date (format-date
                     (local-time:timestamp-to-universal
                      (local-time:timestamp-
                       (local-time:now)
                       days-ago :day)))))
    (loop for active-user in (bknr.datastore:class-instances 'active-user)
          if (and (funcall company-test (company active-user) company)
                  (string>= (active-user-date active-user) start-date))
              collect active-user)))

(defun format-date (ts)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time ts)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun mark-active-user-impl (&key user company (date (get-universal-time)) ip)
  (when (and user company)
    (let ((date (format-date date)))
      (let ((event
              (or (index-get +lookup-index+ (list date user company))
                  (make-instance 'active-user
                                 :user user
                                 :company company
                                 :date date))))
        (when ip
          (setf (gethash ip (active-user-ip-addresses event)) t))
        event))))

(defvar *events* nil)

(defun mark-active-user (&rest args)
  "We don't want to wait on locks in the happy path, and we don't want
reads to depend on writes."
  (unless (%impersonatedp)
    (atomics:atomic-push
     (lambda ()
       (apply #'mark-active-user-impl args))
     *events*)))

(defun %impersonatedp ()
  (and
   (boundp 'hunchentoot:*request*)
   (impersonatedp (make-impersonation))))

(defun flush-events ()
  (bt:with-lock-held (*lock*)
   (let ((events (util/atomics:atomic-exchange *events* nil)))
     (loop for event in events
           do (funcall event)))))

(def-cron flush-events ()
  (flush-events))
