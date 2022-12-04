;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/threading
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:mp :util/fake-mp))
  (:export
   #:call-with-thread-fixes
   #:make-thread
   #:with-safe-interruptable
   #:safe-interrupt-checkpoint
   #:safe-interrupt
   #:log-sentry
   #:*catch-errors-p*
   #:*log-sentry-p*
   #:*extras*
   #:ignore-and-log-errors
   #:*warning-count*)
)
(in-package :util/threading)

(defvar *catch-errors-p* t)
(defvar *log-sentry-p* t)

(defvar *message-handlers* nil)

(defmacro with-message-handlers (name-and-fns &body body)
  (cond
    (name-and-fns
     `(call-with-message-handler
       ',(caar name-and-fns)
        ,(cadar name-and-fns)
       (lambda ()
         (with-message-handlers ,(cdr name-and-fns)
           ,@body))))
    (t
     `(progn ,@body))))

(defun call-with-message-handler (name fn body)
  (let ((*message-handlers*
          (list* (cons name fn)
                 *message-handlers*)))
    (funcall body)))

(defmacro with-safe-interruptable ((&key on-quit) &body body)
  "Create a safe interruptable code. The interrupt can only happen at
checkpoints called by `(safe-interrupte-checkpoint)`"
  `(call-with-safe-interruptable (lambda () ,@body)
                                 :on-quit ,on-quit))

(define-condition safe-interrupt ()
  ())

(defun call-with-safe-interruptable (fn &key on-quit)
  (handler-case
      (with-message-handlers ((quit (lambda ()
                                      (signal 'safe-interrupt))))
          (funcall fn))
    (safe-interrupt ()
      (when on-quit
        (funcall on-quit)))))

(defun safe-interrupt (process)
  (assert (mp:process-p process))
  (send-mail process 'quit))


(defun safe-interrupt-checkpoint ()
  (loop for msg = (mp:process-wait-for-event
                   :no-hang-p t)
        while msg
        do (process-mail msg)))

(defun process-mail (mail)
  (log:info "processing mail: ~A" mail)
  (destructuring-bind (message &rest args) mail
    (let ((handler (a:assoc-value *message-handlers* message)))
      (cond
        (handler
         (apply handler args))
        (t
         (log:warn "No message handler for ~a" message))))))



(defmethod send-mail (process message &rest args)
  (mp:process-send process
                   `(,message ,@args)))

(defun call-with-thread-fixes (fn)
  (funcall fn))

(defvar *warning-count* 0)

(defvar *extras* nil)

(defun build-extras (condition)
  ;; If the extras are nil, it probably means some calback is not
  ;; returning a list
  (ignore-errors
   (loop for extra in *extras*
         appending (ignore-errors (funcall extra condition)))))

(defun %log-sentry (condition)
  #-screenshotbot-oss
  (sentry-client:capture-exception condition :extras (build-extras condition)))

(defmethod log-sentry (condition)
  (when hunchentoot:*catch-errors-p*
    (%log-sentry condition)))

(defmethod log-sentry :around ((warning warning))
  (when (<= (incf *warning-count*) 5)
    (call-next-method)))

(defun maybe-log-sentry (condition)
  (when *log-sentry-p*
    (log-sentry condition)))


(defun funcall-with-sentry-logs (fn)
  (let ((*warning-count* 0))
   (handler-bind ((error #'maybe-log-sentry)
                  (warning #'maybe-log-sentry))
     (funcall fn))))


(defun %invoke-debugger (e)
  ;; mockable version of invoke-debugger
  (invoke-debugger e))

(defun handle-error (e)
  (cond
    ((and
      (not *catch-errors-p*)
      *debugger-hook*)
     ;; Small edge case: SWANK/SLYNK might still try
     ;; to redelegate to the
     ;; default debugger which can
     ;; cause the process to crash
     (%invoke-debugger e))
    (t
     (trivial-backtrace:print-backtrace e)
     (invoke-restart 'ignore-error))))

(def-easy-macro ignore-and-log-errors (&fn fn)
  (restart-case
      (handler-bind ((error #'handle-error))
        (funcall-with-sentry-logs fn))
    (ignore-error ()
      (values))))

(defun make-thread (body &rest args)
  (apply #'bt:make-thread
           (lambda ()
             (ignore-and-log-errors ()
               (funcall body)))
           args))
