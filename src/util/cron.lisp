;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/cron
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:def-cron))
(in-package :util/cron)

#+nil
(defun funcall-with-wrapper (body)
  (restart-case
      (handler-bind ((error (lambda (e)
                              (cond
                                (*debugger-hook*
                                 (invoke-debugger e))
                                (t
                                 (invoke-restart 'cl:abort))))))
        (funcall body))
    (cl:abort ()
      (format t "abort called~%")
      (values))))

(defun call-with-cron-wrapper (fn)
  (util/threading:call-with-thread-fixes fn))

(defmacro def-cron (name (&rest args) &body body)
  `(cl-cron:make-cron-job
    (lambda ()
      (flet ((body () ,@body))
        (call-with-cron-wrapper #'body)))
    :hash-key ',name
    ,@args))
