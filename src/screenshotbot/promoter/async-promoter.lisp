;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/promoter/async-promoter
  (:use #:cl)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class)
  (:import-from #:screenshotbot/model/report
                #:base-acceptable)
  (:import-from #:bknr.indices
                #:hash-index)
  (:import-from #:util/store
                #:with-class-validation)
  (:import-from #:screenshotbot/async
                #:with-screenshotbot-kernel)
  (:import-from #:lparallel.promise
                #:future)
  (:import-from #:util/threading
                #:ignore-and-log-errors)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:util/hash-lock
                #:hash-locked-future
                #:hash-lock)
  (:import-from #:bknr.datastore
                #:class-instances)
  (:import-from #:util/misc
                #:not-null!
                #:?.)
  (:import-from #:screenshotbot/promote-api
                #:maybe-promote
                #:promoter)
  (:import-from #:screenshotbot/user-api
                #:recorder-run-channel)
  (:import-from #:screenshotbot/model/channel
                #:production-run-for)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-merge-base)
  (:import-from #:scheduled-jobs
                #:make-scheduled-job)
  (:import-from #:bknr.indices
                #:destroy-object)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:bknr.datastore
                #:store-object-with-id)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:trigger-promoters-waiting-on-commit
   #:find-or-make-async-promoter
   #:set-timeout))
(in-package :screenshotbot/promoter/async-promoter)

(defvar *hash-lock* (make-instance 'hash-lock))

(def-easy-macro on-channel-thread (channel &fn fn)
  (with-screenshotbot-kernel ()
    (assert channel)
    (hash-locked-future (channel *hash-lock*)
      (ignore-and-log-errors ()
        (funcall fn)))))

(with-class-validation
 (defclass async-promoter (base-acceptable)
   ((run
     :initarg :run
     :initform nil
     :reader promoter-run)
    (merge-base-run
     :initform nil
     :accessor merge-base-run)
    (waiting-on-commit
     :initarg :waiting-on-commit
     :accessor waiting-on-commit)
    (timeout-job
     :initform nil
     :relaxed-object-reference t
     :accessor timeout-job))
   (:metaclass persistent-class)))

(defun promoters-waiting-on-commit (channel commit)
  (loop for promoter in (class-instances 'async-promoter)
        if (and
            (slot-boundp promoter 'waiting-on-commit)
            (eql channel (recorder-run-channel (promoter-run promoter)))
            (?. string-equal (waiting-on-commit promoter) commit))
          collect promoter))

(defun find-async-promoter (class run)
  (loop for promoter in (class-instances 'async-promoter)
        if (and
            (slot-boundp promoter 'run)
            (eql (promoter-run promoter) run)
            (typep promoter class))
          return promoter))

(defmethod merge-base (promoter)
  "Find the merge-commit for this promoter. i.e. the commit that we're
 waiting for in order to do any work."
  (recorder-run-merge-base
   (promoter-run promoter)))

(defgeneric update-status (promoter)
  (:documentation "Called when the state changes to push any updates to external
 services."))

(defun init-merge-base-run (promoter)
  (a:when-let ((merge-base (merge-base promoter)))
    (let ((base-run (production-run-for
                     (recorder-run-channel (promoter-run promoter))
                     :commit merge-base)))
      (cond
        (base-run
         (with-transaction ()
           (setf (merge-base-run promoter) base-run)))
        (t
         (with-transaction ()
           (setf (waiting-on-commit promoter) merge-base)))))))

(defgeneric on-commit-ready (promoter)
  (:method (promoter))
  (:method :before (promoter)
    (init-merge-base-run promoter))
  (:method :after (promoter)
    (update-status promoter)))

(defgeneric on-promote (promoter)
  (:method (promoter))
  (:method :before (promoter)
    (init-merge-base-run promoter))
  (:method :after (promoter)
    (update-status promoter)))

(defmethod trigger-promoters-waiting-on-commit (channel commit)
  (loop for promoter in (promoters-waiting-on-commit channel commit)
        collect (on-channel-thread (channel)
                  (on-commit-ready promoter))))

(defclass wrapper-promoter (promoter)
  ((class :initarg :class
          :reader wrapper-promoter-class)))

(defun find-or-make-async-promoter (type &key (run (error "needs run")))
  (or
   (find-async-promoter type run)
   (make-instance type :run run)))

(defun make-sync-promoter (type)
  (make-instance 'wrapper-promoter
                 :class (not-null! (find-class type))))


(defmethod maybe-promote ((promoter wrapper-promoter) run)
  (let ((async-promoter (find-or-make-async-promoter
                         (wrapper-promoter-class promoter)
                         :run run)))
    (on-channel-thread ((recorder-run-channel run))
      (on-promote async-promoter))))

(defgeneric on-timeout (promoter))

(defmethod %%timeout-callback ((promoter async-promoter))
  (with-transaction ()
    (setf (timeout-job promoter) nil))
  (on-channel-thread ((recorder-run-channel (promoter-run promoter)))
    (on-timeout promoter)))

(defun %timeout-callback (promoter-id)
  (let ((promoter (store-object-with-id promoter-id)))
    (%%timeout-callback promoter)))

(defmethod set-timeout ((self async-promoter) time)
  (cancel-timeout self)
  (let ((job (make-scheduled-job
              :at (+ (get-universal-time) time)
              :function '%timeout-callback
              :args (list (store-object-id self)))))
    (with-transaction ()
      (setf (timeout-job self) job))))

(defmethod cancel-timeout ((self async-promoter))
  (a:when-let ((job (timeout-job self)))
    (bknr.datastore:delete-object job)
    (with-transaction ()
      (setf (timeout-job self) nil))))

(defmethod destroy-object :before ((self async-promoter))
  (a:when-let ((job (timeout-job self)))
   (bknr.datastore:delete-object job)))
