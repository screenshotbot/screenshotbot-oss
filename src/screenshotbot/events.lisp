;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/events
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:lparallel.promise
                #:speculate)
  (:import-from #:screenshotbot/async
                #:with-screenshotbot-kernel)
  (:import-from #:clsql-sys
                #:*sql-stream*)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:util/lists
                #:with-batches)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/events)

(defvar *event-engine* nil)
(defvar *events* nil)

(defclass db-engine ()
  ((connection-spec :initarg :connection-spec
                    :reader connection-spec)
   (database-type :initarg :database-type
                  :reader database-type)))


(defmethod push-event-impl (engine name &rest args)
  nil)

(defclass event ()
  ((name :initarg :name
         :type string
         :reader event-name)
   (extras :initarg :extras
           :initform nil
           :reader event-extras)
   (created-at
    :accessor created-at
    :type local-time:timestamp
    :initform (local-time:now))))

(def-easy-macro on-background (&fn fn)
  (with-screenshotbot-kernel ()
    (speculate
      (funcall fn))))

(def-easy-macro with-db (&binding db engine &fn fn)
  (let ((db (clsql:connect (connection-spec engine)
                           :database-type (database-type engine)
                           :pool nil
                           :make-default nil)))
    (unwind-protect
         (funcall fn db)
      (clsql:disconnect :database db))))

(defclass sql-value-list ()
  ((values :initarg :values
           :reader %values)))

(defmethod clsql-sys::output-sql ((vl sql-value-list) database)
  (loop for val in (%values vl)
        for i from 0
        if (/= i 0)
          do
             (format *sql-stream* ",")
        do
        (clsql-sys::output-sql val database)))

(defun insert-events (events db)
  (when events
   (clsql:insert-records
    :into "event"
    :attributes '("name" "extras" "created_at")
    :values (make-instance
             'sql-value-list
             :values
             (loop for ev in events
                   collect
                   (list
                    (event-name ev)
                    (event-extras ev)
                    (created-at ev))))
    :database db)))

(defmethod push-event-impl ((engine db-engine) name &rest args)
  (let ((ev (make-instance 'event
                           :name (str:downcase name)
                           :extras
                           (json:encode-json-plist-to-string
                            args))))
    (atomics:atomic-push
     ev *events*)))

(defmethod flush-events (engine)
  (values))

(defmethod flush-events ((engine db-engine))
  (with-db (db engine)
    (let ((events))
      (atomics:atomic-update *events* (lambda (old)
                                        (setf events old)
                                        nil))
      (with-batches (events events :batch-size 1000)
        (insert-events events db)))))

(defmethod push-event (name &rest args)
  (apply #'push-event-impl *event-engine* name args))

(def-cron flush-events (:step-min 1)
  (flush-events *event-engine*))
