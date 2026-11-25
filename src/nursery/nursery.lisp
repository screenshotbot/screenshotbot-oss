;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :nursery
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:with-nursery
   #:*catch-errors-p*
   #:nursery-let
   #:start-soon
   #:cancel
   #:cancelledp
   #:checkpoint
   #:nursery-sleep
   #:nursery-error
   #:nursery-error-failures
   #:failure
   #:failure-task-name
   #:failure-condition
   #:failure-backtrace
   #:nursery-closed
   #:task-aborted))
(in-package :nursery)

;;;; Structured concurrency, as described in
;;;; https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/
;;;;
;;;; A thread can only be started in a nursery, and WITH-NURSERY does
;;;; not exit until all the threads started in it are complete. If any
;;;; of them failed, WITH-NURSERY signals a NURSERY-ERROR.
;;;;
;;;; Cancellation is cooperative. A failure, or a call to CANCEL,
;;;; cancels the nursery, but the body and the tasks only exit at their
;;;; next call to CHECKPOINT. We never interrupt a thread, so code that
;;;; never calls CHECKPOINT (or is blocked on I/O) runs to completion,
;;;; and WITH-NURSERY waits for it.

(defvar *scopes* nil
  "The nurseries that the current thread can exit from by throwing,
innermost first: the nursery that the thread is a task of, if any, and
then every WITH-NURSERY that the thread is in the body of.")

(defvar *catch-errors-p* t
  "When this is NIL and there's a *DEBUGGER-HOOK*, an error in a task or
in the body of WITH-NURSERY first invokes the debugger, on the thread
that signaled it, with the stack intact. This is the same as
UTIL/THREADING:*CATCH-ERRORS-P*.

From the debugger, a restart that continues the task (e.g. one that
retries the failing step) means that the error was never a failure.
The ABORT restart that we establish records the error as a failure of
the nursery, just as if this were T.

A task uses the value that this has on the thread that calls
START-SOON, at the time that it's called.")

(defvar *nursery-bound-vars* nil
  "The variables that the current thread has bound with NURSERY-LET, or
inherited from the thread that started it. START-SOON propagates
exactly these, so its cost depends only on how many of them are
bound.")

(defun add-bound-vars (vars bound-vars)
  (dolist (var vars bound-vars)
    (unless (member var bound-vars)
      (push var bound-vars))))

(defmacro nursery-let (bindings &body body)
  "Like LET for special variables, but the bindings propagate to the
tasks started with START-SOON while they're in effect, including tasks
started from those tasks, in any nursery. A plain LET does not
propagate.

A task gets the values that the variables have on the thread that
calls START-SOON, at the time that it's called, in new bindings of its
own. So a SETF of the variable in the task doesn't change the value in
the thread that started it.

The variables are declared special here, so these are always dynamic
bindings, but they should be DEFVARed anyway like any other special
variable, so that other code that refers to them sees them as special."
  (let ((vars (mapcar (lambda (binding)
                        (if (consp binding) (car binding) binding))
                      bindings)))
    `(let (,@bindings
           (*nursery-bound-vars* (add-bound-vars ',vars *nursery-bound-vars*)))
       (declare (special ,@vars))
       ,@body)))

(defclass nursery ()
  ((lock :initform (bt:make-lock "nursery")
         :reader lock)
   (cv :initform (bt:make-condition-variable)
       :reader cv)
   (active-count :initform 0
                 :accessor active-count
                 :documentation "The number of tasks that are not yet complete.")
   (closedp :initform nil
            :accessor closedp)
   (failures :initform nil
             :accessor failures
             :documentation "Most recent failure first")
   (propagate :initarg :propagate
              :initform nil
              :reader propagate)
   (parent :initarg :parent
           :initform nil
           :reader parent
           :documentation "The nursery that the WITH-NURSERY for this nursery was called in, if
any. Cancelling the parent cancels this nursery too.")
   (cancel-requested-p :initform nil
                       :accessor cancel-requested-p
                       :documentation "Whether this nursery itself was cancelled, see CANCELLEDP.")))

(defclass failure ()
  ((task-name :initarg :task-name
              :reader failure-task-name
              :documentation "The name passed to START-SOON, or :BODY for the body of
WITH-NURSERY.")
   (condition :initarg :condition
              :reader failure-condition)
   (backtrace :initarg :backtrace
              :reader failure-backtrace
              :documentation "The backtrace, as a string, of the thread that signaled the
condition, from where it was signaled. By the time the NURSERY-ERROR is
signaled that stack is gone, so this is the only record of it."))
  (:documentation "One failed task of a nursery."))

(defmethod print-object ((self failure) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a: ~a"
            (failure-task-name self)
            (failure-condition self))))

(define-condition nursery-error (error)
  ((failures :initarg :failures
             :reader nursery-error-failures
             :documentation "A list of FAILUREs, in the order that they happened."))
  (:report (lambda (e stream)
             (let ((failures (nursery-error-failures e)))
               (format stream "~d task~:p failed in the nursery:" (length failures))
               (dolist (failure failures)
                 (format stream "~%  ~a: ~a"
                         (failure-task-name failure)
                         (failure-condition failure))))))
  (:documentation "Signaled by WITH-NURSERY once all the tasks are complete, if any of
them failed. This is signaled even if only one task failed, so that
the type of the error doesn't depend on how many tasks failed."))

(define-condition nursery-closed (error)
  ()
  (:report "START-SOON was called on a nursery whose WITH-NURSERY has already exited"))

(define-condition task-aborted (error)
  ()
  (:report "The thread exited before the task could complete"))

(defun current-backtrace ()
  (ignore-errors
   (with-output-to-string (stream)
     (trivial-backtrace:print-backtrace-to-stream stream))))

(defclass sleeper ()
  ((lock :initform (bt:make-lock "nursery-sleeper")
         :reader lock)
   (cv :initform (bt:make-condition-variable)
       :reader cv))
  (:documentation "A thread that's in NURSERY-SLEEP."))

(defvar *sleepers-lock* (bt:make-lock "nursery-sleepers"))

(defvar *sleepers* (make-hash-table :test #'eq)
  "The keys are every SLEEPER, in every nursery. Cancelling is rare, so
cancelling any nursery just wakes up all of them, and each one checks
whether it was its own nursery that was cancelled.")

(defun request-cancel (nursery)
  "Must be called with the lock of NURSERY held."
  (setf (cancel-requested-p nursery) t)
  ;; A sleeper registers itself before it checks CANCELLEDP, so either
  ;; it sees the flag that we just set, or it's in *SLEEPERS*.
  (dolist (sleeper (bt:with-lock-held (*sleepers-lock*)
                     (loop for sleeper being the hash-keys of *sleepers*
                           collect sleeper)))
    ;; The sleeper checks CANCELLEDP with its lock held, so this
    ;; can't happen between that check and its wait.
    (bt:with-lock-held ((lock sleeper))
      (bt:condition-notify (cv sleeper)))))

(defun record-failure (nursery task-name condition backtrace)
  (let ((failure (make-instance 'failure
                                :task-name task-name
                                :condition condition
                                :backtrace backtrace)))
    (bt:with-lock-held ((lock nursery))
      (push failure (failures nursery))
      ;; There's no point in the rest of the nursery carrying on.
      (request-cancel nursery))))

(defun cancel (nursery)
  "Cancel NURSERY, and every nursery nested in it. The body and the
tasks exit at their next call to CHECKPOINT, running their cleanup
forms. This returns immediately, it does not wait for them.

Being cancelled is not a failure: if nothing failed, WITH-NURSERY
returns normally."
  (bt:with-lock-held ((lock nursery))
    (request-cancel nursery))
  (values))

(defun cancelledp (nursery)
  "Whether NURSERY, or a nursery that it's nested in, was cancelled."
  (loop for n = nursery then (parent n)
        while n
        if (cancel-requested-p n)
          return t))

(defun checkpoint ()
  "If the current thread is running in a nursery that was cancelled,
then exit from it non-locally. Otherwise this does nothing, so it's
safe to call from code that might not be running in a nursery.

This is the only place where cancellation takes effect, so a
long-running task should call this regularly. In particular, a task
always runs up to its first CHECKPOINT, even if the nursery was
cancelled before the task was started.

The exit is a THROW and not an error, so that IGNORE-ERRORS or a
HANDLER-CASE in the task cannot swallow it."
  ;; If several of the nurseries are cancelled, we need to exit from
  ;; the outermost one.
  (let ((nursery (find-if #'cancelledp *scopes* :from-end t)))
    (when nursery
      (throw nursery (values)))))

(defun nursery-sleep (seconds)
  "Like SLEEP, but if the current thread is running in a nursery that
gets cancelled, then this wakes up and exits from it, like CHECKPOINT.

This is a CHECKPOINT even if SECONDS is zero, or the nursery was
cancelled before this was called. Outside of a nursery this is just
SLEEP."
  (let ((deadline (+ (get-internal-real-time)
                     (ceiling (* seconds internal-time-units-per-second))))
        (sleeper (make-instance 'sleeper)))
    (bt:with-lock-held (*sleepers-lock*)
      (setf (gethash sleeper *sleepers*) t))
    (unwind-protect
         (bt:with-lock-held ((lock sleeper))
           (loop
             (when (some #'cancelledp *scopes*)
               (return))
             (let ((remaining (- deadline (get-internal-real-time))))
               (unless (plusp remaining)
                 (return))
               ;; This can wake up early, so we loop and check again.
               (bt:condition-wait (cv sleeper) (lock sleeper)
                                  :timeout (/ remaining
                                              (float internal-time-units-per-second))))))
      (bt:with-lock-held (*sleepers-lock*)
        (remhash sleeper *sleepers*))))
  (checkpoint))

(defun call-recording-failures (nursery task-name type fn)
  "Calls FN. If a condition of TYPE escapes from FN, we record it as a
failure of the nursery and return normally. See *CATCH-ERRORS-P* for
when we invoke the debugger first."
  (block nil
    (handler-bind ((serious-condition
                     (lambda (e)
                       (when (typep e type)
                         ;; We're still on the stack that signaled the
                         ;; condition, so this is the backtrace we want.
                         (let ((backtrace (current-backtrace)))
                           (flet ((fail ()
                                    (record-failure nursery task-name e backtrace)
                                    (return (values))))
                             (cond
                               ((and (not *catch-errors-p*)
                                     *debugger-hook*)
                                (restart-case
                                    (invoke-debugger e)
                                  (abort ()
                                    :report (lambda (stream)
                                              (format stream "Abort ~a, and record it as a failure of the nursery"
                                                      (if (eql task-name :body)
                                                          "the body of the WITH-NURSERY"
                                                          (format nil "the nursery task ~a" task-name))))
                                    (fail))))
                               (t
                                (fail)))))))))
      (funcall fn))))

(defun task-done (nursery)
  (bt:with-lock-held ((lock nursery))
    (decf (active-count nursery))
    (bt:condition-notify (cv nursery))))

(defun run-task (nursery name fn)
  (let ((completedp nil))
    (unwind-protect
         (progn
           ;; CHECKPOINT throws to this
           (catch nursery
             (let ((*scopes* (list nursery)))
               ;; Nothing else can handle a condition on this thread,
               ;; so this is not limited to errors.
               (call-recording-failures nursery name 'serious-condition fn)))
           (setf completedp t))
      (unless completedp
        ;; e.g. somebody destroyed this thread. The nursery must still
        ;; find out, or else it will wait forever.
        (record-failure nursery name (make-condition 'task-aborted) nil))
      (task-done nursery))))

(defun start-soon (nursery fn &key (name "nursery-task"))
  "Call FN, with no arguments, in a new thread that belongs to NURSERY.

This can be called from the body of WITH-NURSERY, or from any task
that's running in the nursery. It also works from other threads while
the WITH-NURSERY hasn't exited, but it is advised to not call it from
outside the nursery. Calling it once the WITH-NURSERY has exited
signals NURSERY-CLOSED.

This returns nothing. If the task has a result, then FN has to store
it somewhere."
  ;; This is called on the thread that's starting the task, so this is
  ;; where we can read its bindings.
  (let* ((catch-errors-p *catch-errors-p*)
         (bound-vars *nursery-bound-vars*)
         (symbols (append
                   (remove-if (lambda (var)
                                (or (not (boundp var))
                                    (member var bound-vars)))
                              (propagate nursery))
                   bound-vars))
         (values (mapcar #'symbol-value symbols))
         (startedp nil))
    (bt:with-lock-held ((lock nursery))
      (when (closedp nursery)
        (error 'nursery-closed))
      (incf (active-count nursery)))
    (unwind-protect
         (progn
           (bt:make-thread
            (lambda ()
              (progv symbols values
                (let ((*nursery-bound-vars* bound-vars)
                      (*catch-errors-p* catch-errors-p))
                  (run-task nursery name fn))))
            :name name)
           (setf startedp t))
      (unless startedp
        (task-done nursery)))
    (values)))

(defun close-nursery (nursery)
  "Waits for all the tasks to complete, and then closes the
nursery. Returns the failures in the order that they happened."
  (bt:with-lock-held ((lock nursery))
    (loop until (zerop (active-count nursery))
          do (bt:condition-wait (cv nursery) (lock nursery)))
    ;; A task can start other tasks, but once there are no tasks left
    ;; there's nobody who could start one.
    (setf (closedp nursery) t)
    (reverse (failures nursery))))

(def-easy-macro with-nursery (&binding nursery &key propagate &fn fn)
  "Runs the body with NURSERY bound to a new nursery, which can be passed
to START-SOON. This only exits once the body and all the tasks started
in the nursery are complete, however the body exits.

If the body and all the tasks completed without an error, this returns
the values of the body. Otherwise, this signals a NURSERY-ERROR with
all the failures, including the error from the body, if any. Note that
this means an error from the body is not visible to handlers outside
of the WITH-NURSERY until the tasks are complete, and then only as a
NURSERY-ERROR.

A failure cancels the nursery, see CANCEL. A nursery that was
cancelled without a failure returns normally: with the values of the
body if the body had completed, or else with no values.

If the body exits non-locally for any other reason (RETURN-FROM,
THROW, etc.), and a task failed, then the NURSERY-ERROR is signaled
while unwinding.

PROPAGATE is a list of special variables. A task gets the bindings
that these variables have on the thread that calls START-SOON, at the
time that it's called. None of the other dynamic state (other
bindings, handlers and restarts) is available to a task. Bindings made
with NURSERY-LET always propagate, so those variables don't need to be
listed here."
  (let ((nursery (make-instance 'nursery
                                :propagate propagate
                                :parent (first *scopes*))))
    (unwind-protect
         ;; CHECKPOINT throws to this
         (catch nursery
           (let ((*scopes* (list* nursery *scopes*)))
             (call-recording-failures nursery :body 'error
                                      (lambda ()
                                        (funcall fn nursery)))))
      (let ((failures (close-nursery nursery)))
        (when failures
          (error 'nursery-error :failures failures))))))
