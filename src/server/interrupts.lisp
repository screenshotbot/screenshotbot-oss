;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :server/interrupts
  (:use #:cl)
  #+lispworks
  (:import-from #:system
                #:set-signal-handler)
  (:local-nicknames (#:a #:alexandria)))
(in-package :server/interrupts)

(defclass interrupt-handler ()
  ((lock :initform (bt:make-lock "interrupts")
         :reader lock)
   (cv :initform (bt:make-condition-variable :name "interrupts")
       :reader cv)
   (interruptedp :initform nil
                 :accessor interruptedp)))

;; Currently we don't support signal handlers on
;; non-Lispworks.. temporary though. It's just because we haven't
;; tested it enough.
#-lispworks
(defmethod set-signal-handler (signal (handler (eql t)))
  "Reset signal handler to default"
  (values))

#-lispworks
(defmethod set-signal-handler (signal handler)
  "Set a signal handler"
  (values))

(defun on-interrupt (handler)
  ;; This interrupt could be called in any thread. In particular, it
  ;; might be called inside wait-for-interrupt when we already have
  ;; the lock on handler. So let's start a new thread for this purpose.

  (bt:make-thread
   (lambda ()
    (bt:with-lock-held ((lock handler))
      (setf (interruptedp handler) t)
      (bt:condition-notify (cv handler))))))

(defun wait-for-interrupt (handler)
  (bt:with-lock-held ((lock handler))
    (loop while (not (interruptedp handler))
          do (bt:condition-wait (cv handler) (lock handler)))))

(defun call-unwind-on-interrupt (expr unwind)
  (let ((interrupt-handler (make-instance 'interrupt-handler)))
    (unwind-protect
         (progn
           (set-signal-handler 15 (lambda (&rest args)
                                    (declare (ignore args))
                                    (on-interrupt interrupt-handler)))
           (funcall expr)
           (wait-for-interrupt interrupt-handler)

           ;; Should we call unwind in the unwind-protect?  Maybe. The
           ;; concern is if the initializers failed for some reason,
           ;; we probably don't want to run the unwind code. So
           ;; there's some trickiness here in the API we want to
           ;; expose.
           (funcall unwind))
      (set-signal-handler 15 t))))

(defmacro unwind-on-interrupt (() expr &body unwind)
  `(call-unwind-on-interrupt
    (lambda ()
      ,expr)
    (lambda ()
      ,@unwind)))
