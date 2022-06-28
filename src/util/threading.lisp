;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/threading
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:mp :util/fake-mp))
  (:export
   #:call-with-thread-fixes
   #:make-thread
   #:with-safe-interruptable
   #:safe-interrupt-checkpoint
   #:safe-interrupt
   #:log-sentry)
)
(in-package :util/threading)


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

(defun %log-sentry (condition)
  #-screenshotbot-oss
  (sentry-client:capture-exception condition))

(defmethod log-sentry (condition)
  (when hunchentoot:*catch-errors-p*
    (%log-sentry condition)))

(defmethod log-sentry :around ((warning warning))
  (when (<= (incf *warning-count*) 5)
    (call-next-method)))


(defun funcall-with-sentry-logs (fn)
  (let ((*warning-count* 0))
   (handler-bind ((error #'log-sentry)
                  (warning #'log-sentry))
     (funcall fn))))

(defvar *catch-errors-p* t)

(defun %invoke-debugger (e)
  ;; mockable version of invoke-debugger
  (invoke-debugger e))

(defun handle-error (e)
  (cond
    ((and *catch-errors-p* *debugger-hook*)
     ;; Small edge case: SWANK/SLYNK might still try
     ;; to redelegate to the
     ;; default debugger which can
     ;; cause the process to crash
     (%invoke-debugger e))
    (t
     (trivial-backtrace:print-backtrace e)
     (invoke-restart 'cl:abort))))

(defun make-thread (body &rest args)
  (apply #'bt:make-thread
           (lambda ()
             (handler-bind ((error #'handle-error))
               (funcall-with-sentry-logs body)))
         args))
