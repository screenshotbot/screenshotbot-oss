;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/logger
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:alexandria
                #:when-let)
  (:export
   #:logger
   #:format-log
   #:noop-logger))
(in-package :util/logger)

(defclass logger ()
  ((lock :initform (bt:make-lock "logger lock"))
   (file :initarg :file)))

(defclass noop-logger ()
  ())

(def-easy-macro with-logger-stream (logger &binding stream &fn fn)
  (with-slots (file lock) logger
   (bt:with-lock-held (lock)
     (with-open-file (stream file :if-exists :append :direction :output
                             :if-does-not-exist :create)
       (fn stream)))))

(defun format-ts (stream ts)
  (local-time:format-timestring
   stream
   ts :format `("[" (:hour 2) ":" (:min 2) ":" (:sec 2) "] ")))

(defmethod format-log  ((logger logger)
                        level
                        message
                        &rest args)
  (with-logger-stream (logger stream)
    (format stream "~a: " level)
    (format-ts stream (local-time:now))
    (apply #'format stream message args)
    (format stream "~%")))

(defmethod format-log  ((logger null)
                        level
                        message
                        &rest args)
  ;; Sanity check to make sure our format args are correct in tests
  (apply #'format nil message args))

(defmethod format-log  ((logger noop-logger)
                        level
                        message
                        &rest args)
  (log:info "~a"
            (apply #'format nil message args)))
