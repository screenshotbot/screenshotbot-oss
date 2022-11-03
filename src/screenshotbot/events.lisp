;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/events
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:clsql-sys
                #:sql-ident
                #:*sql-stream*)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:util/lists
                #:with-batches)
  (:import-from #:util/threading
                #:ignore-and-log-errors)
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

(defun format-ts (ts)
  (local-time:format-timestring
   nil ts :format
   '((:year 4) "-"
     (:month  2) "-"
     (:day  2) " "
     (:hour 2) ":"
     (:min  2) ":"
     (:sec  2))))

(defun insert-multiple-items (db table events columns row-builder)
  (when events
    (clsql:insert-records
     :into table
     :attributes (loop for name in columns
                       collect
                       (make-instance 'sql-ident :name name))
     :values (make-instance
              'sql-value-list
              :values
              (loop for ev in events
                    collect
                    (loop for item in (funcall row-builder ev)
                          if (typep item 'local-time:timestamp)
                            collect (format-ts item)
                          else
                            collect item)))
     :database db)))

(defun insert-events (events db)
  (insert-multiple-items db
                         "event"
                         events
                         '("name" "extras" "created_at")
                         (lambda (ev)
                           (list
                            (event-name ev)
                            (event-extras ev)
                            (created-at ev)))))

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
  (when *events*
    (let ((events (util/atomics:atomic-exchange
                   *events* nil)))
      ;; if the connection fails, then we'll drop the events and that's okay
      (with-db (db engine)
        (with-batches (events events :batch-size 1000)
          (insert-events events db))))))

(defmethod push-event (name &rest args)
  (apply #'push-event-impl *event-engine* name args))

(def-easy-macro with-event (name &rest args &fn fn)
  (flet ((push-type (type)
           (apply #'push-event (intern (format nil "~a.~a" name type) "KEYWORD")
                  args)))
   (handler-bind ((error (lambda (e)
                           (declare (ignore e))
                           (push-type "FAILURE"))))
     (let ((ret (funcall fn)))
       (push-type "SUCCESS")
       ret))))


(def-cron flush-events (:step-min 1)
  (ignore-and-log-errors ()
   (flush-events *event-engine*)))

;; (push-event :test-sdf)
