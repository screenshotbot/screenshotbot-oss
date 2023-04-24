;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/remote
  (:use #:cl)
  (:nicknames :screenshotbot/pro/replay/remote)
  (:import-from #:screenshotbot/replay/integration
                #:api-key
                #:schedule-replay-job
                #:run)
  (:import-from #:util/object-id
                #:find-by-oid
                #:oid
                #:object-with-oid)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:bknr.datastore
                #:blob)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:bknr.datastore
                #:store-object)
  (:import-from #:screenshotbot/user-api
                #:company-name
                #:user
                #:can-view
                #:%created-at)
  (:import-from #:screenshotbot/replay/core
                #:write-replay-log
                #:*replay-logs*)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:hunchensocket
                #:websocket-resource)
  (:import-from #:screenshotbot/server
                #:make-thread
                #:staging-p)
  (:import-from #:auto-restart
                #:with-auto-restart)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store
                #:with-class-validation
                #:defindex)
  (:import-from #:util/hash-lock
                #:with-hash-lock-held
                #:hash-lock)
  (:import-from #:util/threading
                #:with-safe-interruptable)
  (:import-from #:screenshotbot/events
                #:with-event)
  (:import-from #:util/store/fset-index
                #:fset-set-compat-index
                #:fset-set-index)
  (:local-nicknames (#:a #:alexandria)
                    (#:frontend #:screenshotbot/replay/frontend)
                    (#:browser-config #:screenshotbot/replay/browser-config))
  (:export
   #:send-remote-run
   #:remote-runs-for-company))
(in-package :screenshotbot/replay/remote)

(defvar *threads* (trivial-garbage:make-weak-hash-table
                   :weakness :value))

(defun make-testing-run ()
  (make-instance 'run
                  :channel "test-channel-2"
                  :sitemap "https://rollins-edu-cla-staging.netlify.app/sitemap.xml"
                  :custom-css "body { background: red !important; padding-top: 10em;} "
                  :api-key "OUZC6OX7RD1BTCWGB77W"
                  :api-secret "8Fv6vuVzLOFU4RvUOgABUzAiyeJCuj2nQuyCaZQ0" :sleep 0 :host "https://staging.screenshotbot.io" :sampling 0.02 :browser-configs (list (make-instance 'browser-config:browser-config :type 'frontend::chrome :name "Chrome"))))



(defparameter *timeout* (* 4 3600))
(defvar *hash-lock* (make-instance 'hash-lock))

(with-auto-restart ()
  (defun send-remote-run (run &key (company (error "Must provide company"))
                                (cleanup (lambda ())))
   (let* ((log-file (make-instance 'log-file))
          (remote-run (make-instance 'remote-run
                                      :company company
                                      :log-file log-file)))

     ;; If the block in with-open-file signals, then the log file will
     ;; be deleted. This prevents that.
     (open (blob-pathname log-file) :direction :probe
          :if-does-not-exist :create)

     (let ((thread
             (make-thread
              (lambda ()
                (with-safe-interruptable (:on-quit
                                          (lambda ()
                                            (with-transaction ()
                                              (write-replay-log "Aborted by user")
                                              (setf (remote-run-status remote-run)
                                                    :user-aborted))))
                  (with-transaction ()
                    (setf (remote-run-status remote-run) :queued))
                  (with-event (:replay-job :company (company-name company))
                   (with-open-file (stream (bknr.datastore:blob-pathname log-file) :direction :output)
                     (handler-bind ((error (lambda (e)
                                             (write-replay-log "Got condition: ~a" e))))
                       (with-hash-lock-held ((remote-run-company remote-run) *hash-lock*)
                         (with-transaction ()
                           (setf (remote-run-status remote-run) :running))
                         (let* ((ret (actually-run run stream)))
                           (log:info "remote run done: ~a" ret)))

                       ;; todo: move to unwind-protect
                       (funcall cleanup))))
                  (with-transaction ()
                    (setf (remote-run-status remote-run) :success))))
              :name (format nil "remote-replay-management-threads for ~a" (bknr.datastore:store-object-id remote-run)))))
       (setf (gethash remote-run *threads*)
             thread))
     remote-run)))

(defun actually-run-serialized (serialized-run &rest args)
  (let ((stream (flexi-streams:make-in-memory-input-stream serialized-run)))
    (let ((run (cl-store:restore stream)))
      (unwind-protect
           (apply #'actually-run run args))
      "done")))

(defun update-remote-run (run)
  (assert (not (donep run)))
  (assert (not (str:emptyp (remote-oid run))))
  (handler-case
      (let ((json (json:decode-json-from-string
                   (dex:get
                    (format nil "~a/get-job-status?oid=~a"
                            (remote-url run)
                            (remote-oid run))))))
        (a:when-let (donep (a:assoc-value json :donep))
          (with-transaction ()
            (setf (donep run) t)
            (setf (remote-run-status run) :success))))
    (dex:http-request-failed ()
      (with-transaction ()
        (setf (donep run) t)))
    (usocket:timeout-error (e))))
;; (log:info "~a" (encode-run (make-testing-run)))

;; (send-remote-run (make-testing-run))

(with-class-validation
 (defclass remote-run (object-with-oid)
   ((remote-url :initarg :remote-url
                :reader remote-url
                :initform nil)
    (company :initarg :company
             :initform nil
             :index-type fset-set-compat-index
             :index-reader remote-runs-for-company
             :reader remote-run-company)
    (log-file :initarg :log-file
              :reader log-file
              :initform nil)
    (oid :initarg :remote-oid
         :initform nil
         :reader remote-oid)
    (donep :initarg :donep
           :initform nil
           :writer (setf donep))
    (status :initarg :status
            :initform :unknown
            :accessor remote-run-status)
    (%created-at :initform 0
                 :initarg :created-at
                 :reader %created-at))
   (:metaclass persistent-class)
   (:default-initargs :created-at (get-universal-time))))

(defmethod can-view ((self remote-run) (user user))
  (let ((company (remote-run-company self)))
    (assert company)
    (check-type company company)
    (can-view (remote-run-company self)
              user)))

(defmethod donep ((run remote-run))
  (let ((thread (gethash run *threads*)))
    (not
     (and thread
          (bt:thread-alive-p thread)))))

(defmethod run-thread-id ((run remote-run))
  (unless (donep run)
    (let ((thread (gethash run *threads*)))
      (values
       (bt:thread-name thread)
       thread))))

(defclass local-run (object-with-oid)
  ((donep :initform nil
          :accessor donep)
   (log-file :initarg :log-file
             :initform nil
             :accessor log-file))
  (:metaclass persistent-class))

(defclass log-file (blob)
  ()
  (:metaclass persistent-class))

(defclass local-run-log-resource (hunchensocket:websocket-resource)
  ((local-run :initarg :local-run
              :reader local-run))
  (:default-initargs :client-class 'local-run-log-client))

(defclass local-run-log-client (hunchensocket:websocket-client)
  ())

(defun find-ws-resource (request)
  (let ((script-name (hunchentoot:script-name request)))
    (log:info "finding resource with: ~a" script-name)
    (when (str:starts-with-p "/wsapp/replay/logs/" script-name)
      (let* ((oid (car (last (str:split "/" script-name))))
             (run (find-by-oid oid)))
        (check-type run (or local-run
                            remote-run))
        (make-instance 'local-run-log-resource
                        :local-run run)))))

(defvar +sleep-time-for-websocket+ 2
  "If the requests are getting heavy, we can increase this to reduce
  how frequently we're hitting our resources")

(defmacro handle-websocket-gracefully (() &body body)
  `(handler-case
       (progn ,@body)
     #+lispworks
     (conditions:stream-closed-error ()
       (log:info "Websocket closed"))))

(defmethod hunchensocket:client-connected ((resource websocket-resource)
                                           client)
  (log:info "client connected!")
  (util:make-thread
   (lambda ()
     (let* ((+len+ 1024) ;; Update this in websocket-logs.js too
            (data (make-array +len+ :element-type 'character
                                    :adjustable t))
            (local-run (local-run resource))
            (log-file (blob-pathname (log-file local-run))))
       (unwind-protect
            (handle-websocket-gracefully ()
              (when (and
                     (staging-p)
                     (equal "thecharmer" (uiop:hostname)))
               (hunchensocket:send-text-message client
                                                (format nil "Log file is: ~a~%" log-file)))
              (handler-case
                  (with-open-file (stream log-file :direction :input)
                    (labels ((send-remaining ()
                               (when (< (file-position stream)
                                        (file-length stream))
                                 (loop
                                   for pos = (read-sequence data stream)
                                   do
                                      (when (< pos +len+)
                                        (adjust-array data pos))
                                      (hunchensocket:send-text-message client data)
                                      (when (< pos +len+)
                                        (adjust-array data +len+)
                                        (return)))
                                 :data-was-sent)))

                      (send-remaining)
                      (loop while (not (donep local-run)) do
                        (sleep +sleep-time-for-websocket+)
                        (unless (send-remaining)
                          (hunchensocket:send-ping client)))
                      (send-remaining)
                      (hunchensocket:send-text-message client "DONE")))
                (file-error (e)
                  (hunchensocket:send-text-message
                   client
                   (format nil "Error opening log file: ~a" e)))))
         (handle-websocket-gracefully ()
          (hunchensocket:close-connection client)))))
   :name (format nil "websocket-log-tail started at ~a" (local-time:now)))
)

(pushnew 'find-ws-resource hunchensocket:*websocket-dispatch-table*)

(defun log-dir ()
  (ensure-directories-exist
   (path:catdir util/store:*object-store* "replay-logs-dir/")))

(defun actually-run (run log-stream)
  (let ((*replay-logs* log-stream))
    (schedule-replay-job run)))


(defun get-run-status (local-run)
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string
   `((:donep . ,(donep local-run))
     (:oid . ,(oid local-run)))))
