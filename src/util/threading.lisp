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
   #:*warning-count*
   #:with-extras
   #:with-tags
   #:wait-for-pool
   #:*propagated-symbols*)
)
(in-package :util/threading)

(defvar *trace-stream* *debug-io*)
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
(defvar *tags* nil)

(defun build-extras (condition &key (extras *extras*))
  ;; If the extras are nil, it probably means some calback is not
  ;; returning a list
  (ignore-errors
   (loop for extra in extras
         appending (ignore-errors (funcall extra condition)))))

(defmacro with-extras-impl (var (&rest pairs) &body body)
  `(let* ((,var (list*
                 ,@ (loop for (name body) in pairs
                          collect
                          `(lambda (e)
                             (declare (ignore e))
                             (list
                              (cons ,name (format nil "~a"
                                                  (progn
                                                    ,body))))))
                    ,var)))
     (progn ,@body)))

(defmacro with-extras ((&rest pairs) &body body)
  `(with-extras-impl *extras* ,pairs ,@body))

(defmacro with-tags ((&rest pairs) &body body)
  `(with-extras-impl *tags* ,pairs ,@body))

(defun %log-sentry (condition)
  #-screenshotbot-oss
  (let ((tags (build-extras condition :extras *tags*)))
    (sentry-client:capture-exception condition :extras (build-extras condition)
                                               :tags tags)))

(defmethod log-sentry (condition)
  (when *catch-errors-p*
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
     (trivial-backtrace:print-backtrace
      e
      :output *trace-stream*)
     (invoke-restart 'ignore-error))))

(def-easy-macro ignore-and-log-errors (&fn fn)
  (restart-case
      (handler-bind ((error #'handle-error))
        (funcall-with-sentry-logs fn))
    (ignore-error ()
      (values))))

(defclass unlimited-pool ()
  ())

(defvar *unlimited-pool* (make-instance' unlimited-pool))

(defvar *propagated-symbols* nil
  "A list of symbols whose value will always be propagated to child
threads.")

(defun make-thread (body &key (pool *unlimited-pool*) name)
  (let ((bindings (loop for sym in *propagated-symbols*
                        if (boundp sym)
                          collect (cons sym (symbol-value sym)))))
    (make-thread-impl
     pool
     (lambda ()
       (ignore-and-log-errors ()
         (with-tags (("hostname" (uiop:hostname)))
           (progv
               (mapcar #'car bindings)
               (mapcar #'cdr bindings)
            (funcall body)))))
     :name name)))

(defmethod make-thread-impl ((self unlimited-pool) fn &key name)
  (bt:make-thread
   fn
   :name name))

(defclass max-pool-job ()
  ((fn :initarg :fn
     :reader fn)
   (name :initarg :name
         :reader job-name)
   (promise :initarg :promise
            :reader job-promise)))

(defclass max-pool ()
  ((max :initarg :max
        :initform 100
        :reader max-count)
   (thread-count :initform 0
                 :reader thread-count)
   (queue :accessor queue
          :initform (fset:empty-seq))
   (lock :initform (bt:make-lock)
         :reader lock))
  (:documentation "A thread pool that has a maximum number of parallel threads running."))

(defmethod wait-for-pool ((self max-pool) &key (timeout 30))
  "Mostly for testing purposes, wait until the pool is inactive"
  (let ((end-time (+ timeout (get-universal-time))))
    (loop until (bt:with-lock-held ((lock self))
                  (= 0 (slot-value self 'thread-count)))
          do
             (when (> (get-universal-time) end-time)
               (error "Pool didn't end in time"))
             (sleep 0.05))))

(defmethod maybe-process-queue ((self max-pool))
  (bt:with-lock-held ((lock self))
    (when (and
           (< (thread-count self) (max-count self))
           (not (fset:empty? (queue self))))
      (let ((next (fset:first (queue self))))
        (incf (slot-value self 'thread-count))
        (lparallel:fulfill (job-promise next)
          (bt:make-thread (lambda ()
                            (unwind-protect
                                 (funcall (fn next))
                              (bt:with-lock-held ((lock self))
                                (decf (slot-value self 'thread-count)))
                              (maybe-process-queue self)))
                          :name (job-name next)))
        (setf (queue self) (fset:less-first (queue self)))))))

(defmethod make-thread-impl ((self max-pool) fn &key name)
  "Unlike the unlimited pool, in this case we'll return a promise of a
thread, not a thread itself."
  (let* ((promise (lparallel:promise))
         (job (make-instance 'max-pool-job
                             :fn fn
                             :name name
                             :promise promise)))
    (bt:with-lock-held ((lock self))
      (setf (queue self)
            (fset:with-last (queue self) job)))
    (maybe-process-queue self)
    promise))

(defvar *timer-lock* (bt:make-lock))
(defvar *timer-cv* (bt:make-condition-variable))
(defvar *timers* (fset:empty-set))
(defvar *next-timer-id* 0)

(defclass timer-entry ()
  ((ts :initarg :ts
       :reader ts)
   (timer-id :initform (incf *next-timer-id*))
   (args :initarg :args
         :reader args)))

(defmethod fset:compare ((a timer-entry) (b timer-entry))
  (fset:compare-slots a b
                      'ts 'timer-id))

(defun schedule-timer (timeout fn &key (pool *unlimited-pool*))
  (bt:with-lock-held (*timer-lock*)
    (let ((at-time (+ (get-universal-time) timeout)))
      (log:debug "Scheduling at ~a" at-time)
      (setf
       *timers*
       (fset:with *timers*
                  (make-instance 'timer-entry
                                 :ts at-time
                                 :args (list fn :pool pool))))
      (when (= 1 (fset:size *timers*))
        ;; We're the first to be added, so create a thread
        (make-thread
         (lambda ()
           (log:debug "Creating timer thread")
           (timer-thread))
         :name "Timer thread"))
      (bt:condition-notify *timer-cv*))))

(defun timer-thread ()
  (bt:with-lock-held (*timer-lock*)
    (loop
      (let ((current-time (get-universal-time)))
        (cond
          ((fset:empty? *timers*)
           ;; We're done for now
           (return-from timer-thread nil))
          (t
           (let ((next (fset:least *timers*)))
             (cond
               ((<= (ts next) current-time)
                (log:debug "Timestamp is ready: ~a for ~a"
                           (ts next) current-time)
                (apply #'make-thread (args next))
                (setf *timers* (fset:less *timers* next)))
               (t
                (let ((secs (- (ts next) current-time)))
                  (log:debug "Sleeping for ~a" secs)
                  (bt:condition-wait *timer-cv* *timer-lock* :timeout secs)))))))))))

(def-easy-macro scheduled-future (timeout &rest args &fn fn)
  (let ((promise (lparallel:promise)))
    (apply #'schedule-timer timeout
           (lambda ()
             (lparallel:fulfill promise
               (funcall fn)))
           args)
    promise))
