;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/ignore-and-log-errors
  (:use :cl)
  (:export :ignore-and-log-errors
   :*ignore-and-log-catch-p*
   :ignored-error-warning))
(in-package :screenshotbot/ignore-and-log-errors)

(defvar *ignore-and-log-catch-p* nil)

(define-condition ignored-error-warning (warning)
  ((error :initarg :error
          :reader ignored-error-warning-error
          :initform nil)))

(defmethod print-object ((self ignored-error-warning) out)
  (format out "#<IGNORED-ERROR-WARNING ~a>" (ignored-error-warning-error self)))

(defun %ignore-and-log-errors (fn)
  ;; todo: actually log
  (ignore-errors
   (handler-bind ((error (lambda (e)
                           (warn 'ignored-error-warning
                                  :error e))))
     (funcall fn))))

(defmacro ignore-and-log-errors (() &body body)
  `(%ignore-and-log-errors (lambda () ,@body)))
