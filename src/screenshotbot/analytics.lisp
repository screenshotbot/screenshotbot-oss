;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :screenshotbot/analytics
    (:use #:cl
          #:alexandria)
  (:import-from #:screenshotbot/ignore-and-log-errors
                #:ignore-and-log-errors)
  (:import-from #:util/cron
                #:def-cron)
  (:export #:push-analytics-event
           #:analytics-event-ts))
(in-package :screenshotbot/analytics)

(defvar *events-lock* (bt:make-lock))
(defvar *events* nil)

(defvar *analytics-log-file* #P "analytics-log-file.log")

(defclass analytics-event ()
  ((ip-address
    :initarg :ip-address)
   (session
    :initarg :session
    :accessor event-session)
   (script-name
    :initarg :script-name)
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
   (bt:with-lock-held (*events-lock*)
     (with-open-file (s *analytics-log-file*
                        :direction :output
                        :if-exists :append
                        :element-type '(unsigned-byte 8)
                        :if-does-not-exist :create)
       (dolist (ev (reverse *events*))
         (when (consp (event-session ev))
           (setf (event-session ev) (car (event-session ev))))
         (setf (writtenp ev) t)
         (cl-store:store ev s))
       (setf *events* nil)
       (finish-output s)))))

(defun all-saved-analytics-events ()
  (with-open-file (s *analytics-log-file*
                     :direction :input
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :create)
    (nreverse
     (loop for x = (ignore-errors
                    (cl-store:restore s))
           while x
           collect x))))

(defun all-analytics-events ()
  (append
   *events*
   (all-saved-analytics-events)))

(defun clean-events ()
  (let ((cell (nthcdr 1000 *events*)))
    (if cell
        (setf (cdr cell) nil))))

(let ((ctr 0))
  (defun push-analytics-event ()
    (let ((ev (make-instance 'analytics-event
                             :ip-address (hunchentoot:real-remote-addr)
                             :user-agent (hunchentoot:user-agent)
                             :session (auth:session-key (auth:current-session))
                             :referrer (hunchentoot:referer)
                             :script-name (hunchentoot:script-name hunchentoot:*request*)
                             :query-string (hunchentoot:query-string*))))
      (bt:with-lock-held (*events-lock*)
        (push ev *events*)
        (incf ctr)
        (when (<= 1000 ctr)
          (setf ctr 0)
          (clean-events))))))

(def-cron write-analytics-events ()
  (write-analytics-events))
