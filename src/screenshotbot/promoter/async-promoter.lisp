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
                #:?.)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:trigger-promoters-waiting-on-commit))
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
   ((channel
     :initarg :channel
     :reader channel)
    (waiting-on-commit
     :initarg :waiting-on-commit
     :accessor waiting-on-commit))
   (:metaclass persistent-class)))

(defun promoters-waiting-on-commit (channel commit)
  (loop for promoter in (class-instances 'async-promoter)
        if (and
            (slot-boundp promoter 'waiting-on-commit)
            (eql channel (channel promoter))
            (?. string-equal (waiting-on-commit promoter) commit))
          collect promoter))

(defgeneric on-commit-ready (promoter))

(defmethod trigger-promoters-waiting-on-commit (channel commit)
  (loop for promoter in (promoters-waiting-on-commit channel commit)
        collect (on-channel-thread (channel)
                  (on-commit-ready promoter))))
