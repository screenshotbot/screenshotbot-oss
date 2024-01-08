;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/events
  (:use #:cl)
  (:nicknames :screenshotbot/events)
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
  (:import-from #:core/installation/installation
                #:installation-domain
                #:*installation*)
  (:import-from #:util/misc
                #:?.)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:event-engine
   #:with-tracing
   #:delete-old-data))
(in-package :util/events)

(defvar *events* nil)

(defun safe-installation ()
  ;; We want the event code to be as fixtureless as possible to all
  ;; callers, so we handle unbound installations right here
  (when (boundp '*installation*)
    *installation*))

(defgeneric event-engine (installation)
  (:method (any)
    nil))

(defclass db-engine ()
  ((connection-spec :initarg :connection-spec
                    :reader connection-spec)
   (database-type :initarg :database-type
                  :reader database-type)))


(defmethod push-event-impl (engine name &rest args)
  nil)

(defmethod delete-old-data (engine)
  nil)

(defclass event ()
  ((name :initarg :name
         :reader event-name)
   (extras :initarg :extras
           :initform nil
           :reader event-extras)
   (created-at
    :accessor created-at
    :initform (local-time:now))))

(def-easy-macro with-db (&binding db engine &fn fn)
  (let ((db (clsql:connect (connection-spec engine)
                           :database-type (database-type engine)
                           :make-default nil
                           :if-exists :new
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
  (let ((hostname
          (uiop:hostname))
        (domain
          (?. installation-domain (safe-installation))))
   (insert-multiple-items db
                          "event"
                          events
                          '("name" "extras" "created_at" "domain" "hostname")
                          (lambda (ev)
                            (list
                             (event-name ev)
                             (event-extras ev)
                             (created-at ev)
                             domain
                             hostname)))))

(defmethod push-event-impl ((engine db-engine) name &rest args)
  (let ((ev (make-instance 'event
                           :name (str:downcase name)
                           :extras
                           (json:encode-json-plist-to-string
                            args))))
    (atomics:atomic-push
     ev *events*)))

(defmethod delete-old-data ((engine db-engine))
  (with-db (db engine)
    (clsql:execute-command
     "delete from event where created_at < date_sub(now(), interval 1 month) "
     :database db)))

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
  (apply #'push-event-impl (event-engine (safe-installation)) name args))

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


(def-cron flush-events (:step-min 2 :only-on-leader nil)
  (ignore-and-log-errors ()
   (flush-events (event-engine (safe-installation)))))

;; (push-event :test-sdf)

(def-easy-macro with-tracing (name &fn fn)
  (let ((start-time (local-time:now)))
    (unwind-protect
         (funcall fn)
      (push-event :trace :name (string name)
                         :time (local-time:timestamp-difference
                                (local-time:now) start-time)))))

(def-cron delete-old-data (:minute 0 :hour 7)
  (delete-old-data (event-engine (safe-installation))))
