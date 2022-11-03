;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :screenshotbot/analytics
    (:use #:cl
          #:alexandria
          #:iterate)
  (:import-from #:screenshotbot/ignore-and-log-errors
                #:ignore-and-log-errors)
  (:import-from #:util/cron
                #:def-cron)
  (:import-from #:util/store
                #:object-store)
  (:import-from #:screenshotbot/events
                #:*event-engine*
                #:insert-multiple-items
                #:db-engine
                #:with-db)
  (:import-from #:util/lists
                #:with-batches)
  (:import-from #:util/misc
                #:?.)
  (:export #:push-analytics-event
           #:analytics-event-ts
           #:analytics-event-script-name
           #:map-analytics-events))
(in-package :screenshotbot/analytics)

(defvar *events* nil)

(clsql:def-view-class analytics-event ()
  ((ip-address
    :initform nil
    :initarg :ip-address)
   (session
    :initarg :session
    :initform nil
    :accessor event-session)
   (script-name
    :initarg :script-name
    :initform nil
    :reader analytics-event-script-name)
   (query-string
    :initarg :query-string
    :initform nil)
   (ts :initform (local-time:now)
       :initarg :ts
       :reader analytics-event-ts)
   (referrer :initarg :referrer)
   (user-agent :initarg :user-agent))
  (:base-table "analytics"))

(defun write-analytics-events ()
  ;; if we enter the debugger with the lock, then the website will be
  ;; down. So let's always, forcefully never enter the debugger.
  (ignore-and-log-errors ()
    (%write-analytics-events)))


(defun make-digest (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256 (flexi-streams:string-to-octets str))))

(defun hash-session-id (ev)
  (when (stringp (event-session ev))
    (setf (event-session ev)
          (make-digest (event-session ev)))))

(defmethod write-analytics-events-to-engine ((engine null) events))

(defmethod write-analytics-events-to-engine ((engine db-engine) events)
  (with-db (db engine)
    (with-batches (events events)
      (insert-multiple-items db "analytics" events
                             '("ip_address" "session" "script_name"
                               "referrer"
                               "user_agent"
                               "query_string" "ts")
                             (lambda (event)
                               (list
                                (slot-value event 'ip-address)
                                (event-session event)
                                (analytics-event-script-name event)
                                (ignore-errors
                                 (slot-value event 'referrer))
                                (ignore-errors
                                 (slot-value event 'user-agent))
                                (slot-value event 'query-string)
                                (analytics-event-ts event)))))))

(defun %write-analytics-events ()
  (let ((old-events (util/atomics:atomic-exchange *events* nil)))
    (write-analytics-events-to-engine *event-engine*
                                      old-events)))

(defun all-analytics-events ()
  (map-analytics-events #'identity))

(defun map-analytics-events (function &key (keep-if (lambda (x) (declare (ignore x)) t))
                                        limit)
  (declare (ignore limit))
  (when *event-engine*
    (let ((res
            (with-db (db *event-engine*)
              (loop for ev in (append *events* (clsql:select 'analytics-event :database db
                                                 :flatp t))
                    if (funcall keep-if ev)
                      collect (funcall function ev)))))
      (cond
        (limit
         (loop for ev in res
               for i below limit
               collect ev))
        (t
         res)))))


(defun push-analytics-event ()
  (let ((ev (make-instance 'analytics-event
                            :ip-address (hunchentoot:real-remote-addr)
                            :user-agent (hunchentoot:user-agent)
                            :session (car (auth:session-key (auth:current-session)))
                            :referrer (hunchentoot:referer)
                            :script-name (hunchentoot:script-name hunchentoot:*request*)
                            :query-string (hunchentoot:query-string*))))
    (atomics:atomic-push ev *events*)))

(def-cron write-analytics-events ()
  (write-analytics-events))
