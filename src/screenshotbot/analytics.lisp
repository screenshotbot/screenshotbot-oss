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
  (:export #:push-analytics-event
           #:analytics-event-ts
           #:analytics-event-script-name
           #:map-analytics-events))
(in-package :screenshotbot/analytics)

(defvar *events-lock* (bt:make-lock))
(defvar *events* nil)

(defun analytics-log-file ()
  (ensure-directories-exist
   (path:catfile (object-store) "analytics/access-log")))

(defclass analytics-event ()
  ((ip-address
    :initarg :ip-address)
   (session
    :initarg :session
    :accessor event-session)
   (script-name
    :initarg :script-name
    :reader analytics-event-script-name)
   (query-string
    :initarg :query-string
    :initform nil)
   (writtenp
    :initarg :writtenp
    :initform nil
    :accessor writtenp)
   (ts :initform (get-universal-time)
       :initarg :ts
       :reader analytics-event-ts)
   (referrer :initarg :referrer)
   (user-agent :initarg :user-agent)))

(defun write-analytics-events ()
  ;; if we enter the debugger with the lock, then the website will be
  ;; down. So let's always, forcefully never enter the debugger.
  (ignore-and-log-errors ()
    (%write-analytics-events)))


(defmacro atomic-exchange (place new-val)
  #-lispworks
  `(bt:with-lock-held (*events-lock*)
     (let ((old-value ,place))
       (setf ,place ,new-val)
       old-value))
  #+lispworks
  `(system:atomic-exchange ,place ,new-val))


(defun %write-analytics-events ()
  (let ((old-events (atomic-exchange *events* nil)))
    (bt:with-lock-held (*events-lock*)
     (with-open-file (s (analytics-log-file)
                        :direction :output
                        :if-exists :append
                        :element-type '(unsigned-byte 8)
                        :if-does-not-exist :create)
       (dolist (ev (nreverse old-events))
         (when (consp (event-session ev))
           (setf (event-session ev) (car (event-session ev))))
         (setf (writtenp ev) t)
         (cl-store:store ev s))
       (finish-output s)))))

(defun all-saved-analytics-events ()
  (read-log-file (analytics-log-file)))

(defun read-log-file (log-file)
  (with-open-file (s log-file
                   :direction :input
                   :element-type '(unsigned-byte 8)
                   :if-does-not-exist :create)
    (nreverse
     (loop for x = (ignore-errors
                    (cl-store:restore s))
           while x
           collect x))))

(defun all-analytics-events ()
  (map-analytics-events #'identity))

(defun map-analytics-events (function &key (keep-if (lambda (x) (declare (ignore x)) t))
                                        limit)
  (let ((count 0)
        ret)
    (declare (type fixnum count))
    (flet ((keep-looking? ()
             (or (null limit) (< count limit)))
           (maybe-send (ev)
             (when (funcall keep-if ev)
               (incf count)
               (push (funcall function ev) ret))))
      (iter (for ev in *events*)
        (while (keep-looking?))
        (maybe-send ev))
      (flet ((process-log-file (log-file)
               (iter (for ev in (read-log-file log-file))
                 (while (keep-looking?))
                 (maybe-send ev))))
        (process-log-file (analytics-log-file))
        (iter (for i from 0)
          (while (keep-looking?))
          (let ((log-file (format nil "~a.~d" (namestring (analytics-log-file)) i)))
            (while (path:-e log-file))
            (process-log-file log-file))))
      (nreverse ret))))


(defun push-analytics-event ()
  (let ((ev (make-instance 'analytics-event
                            :ip-address (hunchentoot:real-remote-addr)
                            :user-agent (hunchentoot:user-agent)
                            :session (auth:session-key (auth:current-session))
                            :referrer (hunchentoot:referer)
                            :script-name (hunchentoot:script-name hunchentoot:*request*)
                            :query-string (hunchentoot:query-string*))))
    #+lispworks
    (system:atomic-push ev *events*) ;; micro optimization :/
    #-lispworks
    (bt:with-lock-held (*events-lock*)
      (push ev *events*))))

(def-cron write-analytics-events ()
  (write-analytics-events))
