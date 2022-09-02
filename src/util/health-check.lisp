;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/health-check
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:def-health-check
   #:run-health-checks))
(in-package :util/health-check)

(defvar *checks* nil)

(defclass health-check ()
  ((name :initarg :name
         :reader health-check-name)
   (slow :initarg :slow)
   (function :initarg :function
             :reader health-check-function)))

(defmacro def-health-check (name (&key slow) &body body)
  `(setf (a:assoc-value *checks* ',name)
         (make-instance 'health-check
                         :name ',name
                         :slow ',slow
                         :function (lambda ()
                                     ,@body))))

(defun call-health-check (health-check &key out)
  (handler-case
      (progn
        (format out "Checking ~a... " (health-check-name health-check))
        (finish-output out)
        (funcall (health-check-function health-check))
        (format out "SUCCESS~%")
        (finish-output out)
        t)
    (error (e)
      (format out "FAILURE [~a]~%" e)
      (finish-output out)
      nil)))


(defun run-health-checks (&key (out *terminal-io*))
  (loop for health-check in (mapcar 'cdr (reverse *checks*))
        if (not (call-health-check health-check :out out))
          collect (health-check-name health-check) into failed
        finally
           (progn
             (cond
               (failed
                (format out "=======================~%")
                (format out "HEALTH CHECKS FAILED!!!~%")
                (format out "=======================~%"))
               (t
                (format out "All health checks done~%")))
             (return
               (values (not failed) failed)))))

(def-health-check sanity-test ()
  (values))

;; ;; (run-health-checks)
