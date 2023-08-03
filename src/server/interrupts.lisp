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
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:unwind-on-interrupt))
(in-package :server/interrupts)

;; Currently we don't support signal handlers on
;; non-Lispworks.. temporary though. It's just because we haven't
;; tested it enough.
#-lispworks
(defmethod set-signal-handler (signal (handler (eql nil)))
  "Reset signal handler to default"
  (values))

#-lispworks
(defmethod set-signal-handler (signal handler)
  "Set a signal handler"
  (values))

(define-condition interrupted-by-signal-handler (error)
  ()
  (:report "Interrupted by signal handler"))

(defun on-interrupt (thread)
  ;; This interrupt could be called in any thread. In particular, it
  ;; might be called inside wait-for-interrupt when we already have
  ;; the lock on handler. So let's start a new thread for this purpose.
  (bt:make-thread
   (lambda ()
     (bt:interrupt-thread thread
                          (lambda ()
                            #+lispworks
                            (progn
                              (format t "Interrupting at: ~%")
                              (dbg:output-backtrace :brief))
                            (error 'interrupted-by-signal-handler))))))

(defun call-unwind-on-interrupt (expr)
  (let ((signals (list 15 #|SIGTERM|#
                       2 #|SIGINT|#))
        (thread (bt:current-thread) #| The thread we'll interrupt |#))
    (flet ((prepare-interrupts ()
             (dolist (sig signals)
               (set-signal-handler sig (lambda (&rest args)
                                         (declare (ignore args))
                                         (format t "Got signal~%")
                                         (on-interrupt thread))))))
      (handler-case
          (unwind-protect
               (progn
                 (prepare-interrupts)
                 ;; Notice that expr won't be interrupted midway, we'll only
                 ;; interrupt at the end of this call.
                 (funcall expr)
                 (log:info "Waiting for interrupts")

                 (prepare-interrupts))
            (dolist (sig signals)
              (set-signal-handler sig nil)))
        (interrupted-by-signal-handler (e)
          (format t "Unwinded from signal handler~%")
          (values))))))

(defmacro unwind-on-interrupt (() &body expr)
  `(call-unwind-on-interrupt
    (lambda ()
      ,@expr)))
