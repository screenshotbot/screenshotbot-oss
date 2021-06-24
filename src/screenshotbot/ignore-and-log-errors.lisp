;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/ignore-and-log-errors
  (:use :cl)
  (:export :ignore-and-log-errors
           :*ignore-and-log-catch-p*))
(in-package :screenshotbot/ignore-and-log-errors)

(defvar *ignore-and-log-catch-p* nil)

(defun %ignore-and-log-errors (fn)
  (ignore-errors
   ;; todo: actually log
   (let ((catch-p (and *ignore-and-log-catch-p*
                       hunchentoot:*catch-errors-p*)))
    (handler-bind ((error (lambda (e)
                            (cond
                              ((and
                                (not *debugger-hook*)
                                catch-p)
                               ;; if we don't have a debugger attached, only then
                               ;; send it over to Sentry.
                               #-screenshotbot-oss
                               (sentry-client:capture-exception e))
                              ((not catch-p)
                               (invoke-debugger e))
                              (t
                               #-screenshotbot-oss
                               (sentry-client:capture-exception e))))))
      (funcall fn)))))

(defmacro ignore-and-log-errors (() &body body)
  `(%ignore-and-log-errors (lambda () ,@body)))
