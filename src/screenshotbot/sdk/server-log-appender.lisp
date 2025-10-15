;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/server-log-appender
  (:use #:cl)
  (:import-from #:log4cl-impl
                #:appender-do-append)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context
                #:api-feature-enabled-p)
  (:import-from #:util/request
                #:http-request)
  (:import-from #:screenshotbot/sdk/hostname
                #:format-api-url)
  (:import-from #:util/threading
                #:make-thread
                #:max-pool)
  (:import-from #:util/reused-ssl
                #:with-reused-ssl)
  (:local-nicknames (#:api-context #:screenshotbot/sdk/api-context)))
(in-package :screenshotbot/sdk/server-log-appender)


(defclass server-log-appender (log4cl:stream-appender)
  ((stream :initarg :stream
           :reader log4cl:appender-stream)))

(defun make-server-log-appender (api-context)
  (make-instance 'server-log-appender
                 :stream (make-instance 'cli-log-stream :api-context api-context)))

(defvar *in-append* nil)

(defclass cli-log-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((api-context :initarg :api-context
                :reader api-context)
   (lock :initform (bt:make-lock)
         :reader lock)
   (buffer :initarg :buffer
           :reader buffer
           :initform (make-string-output-stream))))

(defmethod trivial-gray-streams:stream-write-char ((self cli-log-stream) ch)
  (bt:with-lock-held ((lock self))
    (write-char ch (buffer self))))

(defmethod trivial-gray-streams:stream-write-string ((Self cli-log-stream) string &optional start end)
  (bt:with-lock-held ((lock self))
    (write-string string (buffer self) :start start :end end)))

(defmethod trivial-gray-streams:stream-line-column ((self cli-log-stream))
  nil)

(defmethod trivial-gray-streams:stream-finish-output ((self cli-log-stream))
  (bt:with-lock-held ((lock self))
   (let ((buffer (get-output-stream-string (buffer self))))
     (unless (str:emptyp buffer)
       (%write-log
        (api-context self)
        buffer)))))

(defun %write-log (api-context body)
  (with-reused-ssl ((api-context:engine api-context)) ;; avoid a warning when called from background threads
    (http-request
     (format-api-url api-context  "/api/cli-log")
     :method :post
     :basic-authorization (list (api-context:key api-context)
                                (api-context:secret api-context))
     :content body
     :engine (api-context:engine api-context )
     :content-type "application/text")))



(defparameter *stream*
  (make-instance 'cli-log-stream
                 :api-context (make-instance 'api-context
                                             :key (uiop:getenv "SCREENSHOTBOT_API_KEY")
                                             :secret (uiop:getenv "SCREENSHOTBOT_API_SECRET")
                                             :hostname "https://staging.screenshotbot.io")))

(defparameter *test-appender*
  (make-server-log-appender
   (make-instance 'api-context
                  :key (uiop:getenv "SCREENSHOTBOT_API_KEY")
                  :secret (uiop:getenv "SCREENSHOTBOT_API_SECRET")
                  :hostname "https://staging.screenshotbot.io")))


#+nil
(log4cl:add-appender log4cl:*root-logger*
                            *test-appender*)

;; (log:info "hello")
;; (dotimes (i 10) (log:info "hello"))
;; (log4cl:flush-all-appenders)

;; (format *stream* "hello world~%")
;; (finish-output *stream*)

;; (close *stream*)

