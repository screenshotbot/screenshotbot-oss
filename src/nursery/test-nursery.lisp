;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :nursery/test-nursery
  (:use #:cl
        #:fiveam)
  (:import-from #:nursery
                #:*catch-errors-p*
                #:cancel
                #:cancelledp
                #:checkpoint
                #:failure-backtrace
                #:failure-condition
                #:failure-task-name
                #:nursery-closed
                #:nursery-error
                #:nursery-error-failures
                #:nursery-let
                #:nursery-sleep
                #:start-soon
                #:task-aborted
                #:with-nursery))
(in-package :nursery/test-nursery)

(fiveam:def-suite* :nursery/test-nursery)

(defvar *test-var* :global)

(defvar *test-nursery-var* :global)

(defvar *test-unbound-nursery-var*)

(define-condition my-error (error)
  ())

(define-condition my-other-error (error)
  ())

(defun failures-of (fn)
  "Calls FN, which must signal a NURSERY-ERROR, and returns its failures."
  (handler-case
      (progn
        (funcall fn)
        (error "expected a nursery-error"))
    (nursery-error (e)
      (nursery-error-failures e))))

(test returns-the-values-of-the-body
  (is (equal '(:one :two)
             (multiple-value-list
              (with-nursery (nursery)
                (start-soon nursery (lambda ()))
                (values :one :two))))))

(test waits-for-the-tasks
  (let ((count 0)
        (lock (bt:make-lock)))
    (with-nursery (nursery)
      (dotimes (i 5)
        (start-soon nursery (lambda ()
                              (sleep 0.1)
                              (bt:with-lock-held (lock)
                                (incf count))))))
    (is (eql 5 count))))

(test a-task-can-start-other-tasks
  (let ((result nil))
    (with-nursery (nursery)
      (start-soon nursery
                  (lambda ()
                    (start-soon nursery
                                (lambda ()
                                  (sleep 0.1)
                                  (setf result :done))))))
    (is (eql :done result))))

(test failed-task-is-signaled-as-a-nursery-error
  (let ((failures (failures-of
                   (lambda ()
                     (with-nursery (nursery)
                       (start-soon nursery
                                   (lambda ()
                                     (error 'my-error))
                                   :name "my-task"))))))
    (is (eql 1 (length failures)))
    (let ((failure (first failures)))
      (is (equal "my-task" (failure-task-name failure)))
      (is (typep (failure-condition failure) 'my-error))
      (is (stringp (failure-backtrace failure)))
      (is (not (equal "" (failure-backtrace failure)))))))

(test task-without-a-checkpoint-completes-when-a-task-fails
  (let ((result nil))
    (signals nursery-error
      (with-nursery (nursery)
        (start-soon nursery (lambda ()
                              (error 'my-error)))
        (start-soon nursery (lambda ()
                              (sleep 0.1)
                              (setf result :done)))))
    (is (eql :done result))))

(test all-the-failures-are-reported
  (let ((failures (failures-of
                   (lambda ()
                     (with-nursery (nursery)
                       (start-soon nursery (lambda ()
                                             (error 'my-error)))
                       (start-soon nursery (lambda ()
                                             (sleep 0.1)
                                             (error 'my-other-error))))))))
    (is (equal '(my-error my-other-error)
               (mapcar (lambda (failure)
                         (type-of (failure-condition failure)))
                       failures)))))

(test error-in-the-body-waits-for-the-tasks
  (let* ((result nil)
         (failures (failures-of
                    (lambda ()
                      (with-nursery (nursery)
                        (start-soon nursery (lambda ()
                                              (sleep 0.1)
                                              (setf result :done)))
                        (error 'my-error))))))
    (is (eql :done result))
    (is (eql 1 (length failures)))
    (is (eql :body (failure-task-name (first failures))))
    (is (typep (failure-condition (first failures)) 'my-error))))

(test error-handled-inside-the-body-is-not-a-failure
  (is (eql :handled
           (with-nursery (nursery)
             (start-soon nursery (lambda ()))
             (handler-case
                 (error 'my-error)
               (my-error ()
                 :handled))))))

(test error-handled-inside-a-task-is-not-a-failure
  (finishes
    (with-nursery (nursery)
      (start-soon nursery (lambda ()
                            (ignore-errors
                             (error 'my-error)))))))

(test non-local-exit-waits-for-the-tasks
  (let ((result nil))
    (is (eql :early
             (block nil
               (with-nursery (nursery)
                 (start-soon nursery (lambda ()
                                       (sleep 0.1)
                                       (setf result :done)))
                 (return :early)))))
    (is (eql :done result))))

(test non-local-exit-still-signals-the-failures
  (signals nursery-error
    (block nil
      (with-nursery (nursery)
        (start-soon nursery (lambda ()
                              (error 'my-error)))
        (return :early)))))

(test start-soon-on-a-closed-nursery
  (let ((escaped nil))
    (with-nursery (nursery)
      (setf escaped nursery))
    (signals nursery-closed
      (start-soon escaped (lambda ())))))

(test tasks-get-the-propagated-bindings
  (let ((results nil)
        (lock (bt:make-lock)))
    (flet ((record ()
             (let ((value *test-var*))
               (bt:with-lock-held (lock)
                 (push value results)))))
      (let ((*test-var* :bound))
        (with-nursery (nursery :propagate '(*test-var*))
          (start-soon nursery
                      (lambda ()
                        (record)
                        ;; A task started by a task gets them too
                        (start-soon nursery #'record)))))
      (is (equal '(:bound :bound) results))
      (setf results nil)
      (let ((*test-var* :bound))
        (with-nursery (nursery)
          (start-soon nursery #'record)))
      (is (equal '(:global) results)))))

(test nursery-let-binds-like-let
  (is (equal '(:bound :global)
             (list
              (nursery-let ((*test-nursery-var* :bound))
                *test-nursery-var*)
              *test-nursery-var*))))

(test nursery-let-propagates-to-tasks-and-nested-nurseries
  (let ((results nil)
        (lock (bt:make-lock)))
    (flet ((record ()
             (let ((value *test-nursery-var*))
               (bt:with-lock-held (lock)
                 (push value results)))))
      (nursery-let ((*test-nursery-var* :bound))
        (with-nursery (nursery)
          (start-soon nursery
                      (lambda ()
                        (record)
                        ;; A task started by a task gets it too
                        (start-soon nursery #'record)
                        ;; And so does a task in a nursery nested in a
                        ;; task, even though that thread didn't bind it
                        ;; itself
                        (with-nursery (child)
                          (start-soon child #'record))))
          ;; And a nursery nested in the body
          (with-nursery (child)
            (start-soon child #'record))))
      (is (equal '(:bound :bound :bound :bound) results)))))

(test plain-let-does-not-propagate
  (let ((result nil))
    (let ((*test-nursery-var* :bound))
      (with-nursery (nursery)
        (start-soon nursery
                    (lambda ()
                      (setf result *test-nursery-var*)))))
    (is (eql :global result))))

(test tasks-get-the-value-at-the-time-of-start-soon
  (let ((results nil)
        (lock (bt:make-lock)))
    (flet ((record ()
             (let ((value *test-nursery-var*))
               (bt:with-lock-held (lock)
                 (push value results)))))
      (with-nursery (nursery)
        (nursery-let ((*test-nursery-var* :first))
          (start-soon nursery #'record)
          ;; A SETF of the binding is seen by later tasks, as it would
          ;; be by anything else in the dynamic extent of the binding
          (setf *test-nursery-var* :second)
          (start-soon nursery #'record)
          ;; And the inner binding wins
          (nursery-let ((*test-nursery-var* :third))
            (start-soon nursery #'record)))
        (start-soon nursery #'record))
      (is (equal '(:first :global :second :third)
                 (sort results #'string< :key #'symbol-name))))))

(test setf-in-a-task-does-not-change-the-parent-binding
  (nursery-let ((*test-nursery-var* :bound))
    (with-nursery (nursery)
      (start-soon nursery
                  (lambda ()
                    (setf *test-nursery-var* :changed))))
    (is (eql :bound *test-nursery-var*))))

(test nursery-let-of-an-unbound-var
  (let ((result :not-run))
    (with-nursery (nursery)
      (start-soon nursery
                  (lambda ()
                    (setf result (boundp '*test-unbound-nursery-var*)))))
    (is (eql nil result))
    (nursery-let ((*test-unbound-nursery-var* :bound))
      (with-nursery (nursery)
        (start-soon nursery
                    (lambda ()
                      (setf result *test-unbound-nursery-var*)))))
    (is (eql :bound result))))

(test nursery-let-and-propagate-together
  (let ((result nil))
    (let ((*test-var* :bound-too))
      (nursery-let ((*test-nursery-var* :bound))
        (with-nursery (nursery :propagate '(*test-nursery-var* *test-var*))
          (start-soon nursery
                      (lambda ()
                        (setf result (list *test-nursery-var* *test-var*)))))))
    (is (equal '(:bound :bound-too) result))))

(test nursery-let-of-a-variable-that-was-never-defvared
  (let ((result nil))
    (nursery-let ((test-never-defvared :bound))
      (with-nursery (nursery)
        (start-soon nursery
                    (lambda ()
                      (setf result (symbol-value 'test-never-defvared))))))
    (is (eql :bound result))
    (is (not (boundp 'test-never-defvared)))))

(defvar *debugged-lock* (bt:make-lock))

(defun call-with-debugger-hook (restart fn &optional (debugged (make-array 0 :adjustable t :fill-pointer t)))
  "Calls FN with a *DEBUGGER-HOOK* that pushes the condition onto the
vector DEBUGGED and then invokes RESTART. Returns DEBUGGED, if FN
returns."
  (let ((*debugger-hook* (lambda (condition hook)
                           (declare (ignore hook))
                           (bt:with-lock-held (*debugged-lock*)
                             (vector-push-extend condition debugged))
                           (invoke-restart restart)))
        ;; SBCL runs this before *DEBUGGER-HOOK*, and with
        ;; --disable-debugger (as in CI) it quits.
        #+sbcl
        (sb-ext:*invoke-debugger-hook* nil))
    (funcall fn))
  debugged)

(test retrying-from-the-debugger-is-not-a-failure
  (let ((attempts 0)
        (debugged nil))
    (let ((*catch-errors-p* nil))
      (with-nursery (nursery)
        (start-soon nursery
                    (lambda ()
                      (setf debugged
                            (call-with-debugger-hook
                             'retry
                             (lambda ()
                               (loop
                                 (restart-case
                                     (progn
                                       (when (< (incf attempts) 3)
                                         (error 'my-error))
                                       (return))
                                   (retry ()))))))))))
    (is (eql 3 attempts))
    (is (eql 2 (length debugged)))
    (is (typep (aref debugged 0) 'my-error))))

(test aborting-from-the-debugger-records-the-failure
  (let* ((debugged (make-array 0 :adjustable t :fill-pointer t))
         (failures (failures-of
                    (lambda ()
                      (let ((*catch-errors-p* nil))
                        (with-nursery (nursery)
                          (start-soon nursery
                                      (lambda ()
                                        (call-with-debugger-hook
                                         'abort
                                         (lambda ()
                                           (error 'my-error))
                                         debugged))
                                      :name "my-task")))))))
    (is (eql 1 (length failures)))
    (is (equal "my-task" (failure-task-name (first failures))))
    (is (typep (failure-condition (first failures)) 'my-error))
    (is (stringp (failure-backtrace (first failures))))
    (is (eql 1 (length debugged)))))

(test aborting-the-body-from-the-debugger-records-the-failure
  (let ((failures (failures-of
                   (lambda ()
                     (let ((*catch-errors-p* nil))
                       (call-with-debugger-hook
                        'abort
                        (lambda ()
                          (with-nursery (nursery)
                            (error 'my-error)))))))))
    (is (eql 1 (length failures)))
    (is (eql :body (failure-task-name (first failures))))
    (is (typep (failure-condition (first failures)) 'my-error))))

(test catch-errors-p-does-not-invoke-the-debugger
  (let ((debugged (make-array 0 :adjustable t :fill-pointer t)))
    (failures-of
     (lambda ()
       (with-nursery (nursery)
         (start-soon nursery
                     (lambda ()
                       (call-with-debugger-hook
                        'abort
                        (lambda ()
                          (error 'my-error))
                        debugged))))))
    (is (eql 0 (length debugged)))))

(test no-debugger-hook-records-the-failure
  (let ((failures (failures-of
                   (lambda ()
                     (let ((*catch-errors-p* nil))
                       (with-nursery (nursery)
                         (start-soon nursery
                                     (lambda ()
                                       (let ((*debugger-hook* nil))
                                         (error 'my-error))))))))))
    (is (eql 1 (length failures)))
    (is (typep (failure-condition (first failures)) 'my-error))))

(test destroyed-thread-does-not-hang-the-nursery
  (let* ((thread nil)
         (lock (bt:make-lock))
         (cv (bt:make-condition-variable))
         (failures
           (failures-of
            (lambda ()
              (with-nursery (nursery)
                (start-soon nursery
                            (lambda ()
                              (bt:with-lock-held (lock)
                                (setf thread (bt:current-thread))
                                (bt:condition-notify cv))
                              (sleep 60)))
                (bt:with-lock-held (lock)
                  (loop until thread
                        do (bt:condition-wait cv lock)))
                (bt:destroy-thread thread))))))
    (is (eql 1 (length failures)))
    (is (typep (failure-condition (first failures)) 'task-aborted))))

(defun run-until-cancelled ()
  (loop
    (checkpoint)
    (sleep 0.01)))

(test checkpoint-outside-of-a-nursery-does-nothing
  (finishes (checkpoint)))

(test cancel-stops-the-tasks-at-a-checkpoint
  (let ((cleaned-up-p nil))
    (finishes
      (with-nursery (nursery)
        (start-soon nursery
                    (lambda ()
                      (unwind-protect
                           (run-until-cancelled)
                        (setf cleaned-up-p t))))
        (is-false (cancelledp nursery))
        (cancel nursery)
        (is-true (cancelledp nursery))))
    (is-true cleaned-up-p)))

(test body-that-completed-before-the-cancel-returns-its-values
  (is (equal '(:one :two)
             (multiple-value-list
              (with-nursery (nursery)
                (start-soon nursery
                            (lambda ()
                              (sleep 0.1)
                              (cancel nursery)))
                (values :one :two))))))

(test cancelled-body-returns-no-values
  (let ((reached-p nil))
    (is (equal nil
               (multiple-value-list
                (with-nursery (nursery)
                  (cancel nursery)
                  (checkpoint)
                  (setf reached-p t)
                  :not-returned))))
    (is-false reached-p)))

(test task-started-after-the-cancel-runs-up-to-its-first-checkpoint
  (let ((result nil))
    (with-nursery (nursery)
      (cancel nursery)
      (start-soon nursery (lambda ()
                            (setf result :started)
                            (checkpoint)
                            (setf result :not-cancelled))))
    (is (eql :started result))))

(test failed-task-cancels-the-other-tasks-and-the-body
  (let ((failures (failures-of
                   (lambda ()
                     (with-nursery (nursery)
                       (start-soon nursery #'run-until-cancelled)
                       (start-soon nursery (lambda ()
                                             (error 'my-error))
                                   :name "failing")
                       (run-until-cancelled))))))
    ;; Being cancelled is not a failure
    (is (equal '("failing")
               (mapcar #'failure-task-name failures)))))

(test failed-body-cancels-the-tasks
  (let ((failures (failures-of
                   (lambda ()
                     (with-nursery (nursery)
                       (start-soon nursery #'run-until-cancelled)
                       (error 'my-error))))))
    (is (equal '(:body)
               (mapcar #'failure-task-name failures)))))

(test cancellation-is-not-swallowed-by-error-handlers
  (finishes
    (with-nursery (nursery)
      (start-soon nursery
                  (lambda ()
                    (loop
                      (ignore-errors
                       (checkpoint))
                      (handler-case
                          (checkpoint)
                        (error ()
                          nil))
                      (sleep 0.01))))
      (cancel nursery))))

(test cancelling-the-outer-nursery-cancels-the-inner-one
  (let ((after-inner-p nil))
    (finishes
      (with-nursery (outer)
        (start-soon outer
                    (lambda ()
                      (with-nursery (inner)
                        (start-soon inner #'run-until-cancelled)
                        (run-until-cancelled))
                      ;; The task of the outer nursery must exit too,
                      ;; and not just the body of the inner one.
                      (setf after-inner-p t)))
        (sleep 0.1)
        (cancel outer)))
    (is-false after-inner-p)))

(test cancelling-the-inner-nursery-does-not-cancel-the-outer-one
  (is (eql :outer-completed
           (with-nursery (outer)
             (with-nursery (inner)
               (start-soon inner #'run-until-cancelled)
               (cancel inner)
               (run-until-cancelled))
             (is-false (cancelledp outer))
             (checkpoint)
             :outer-completed))))

(defun elapsed-seconds (fn)
  (let ((start (get-internal-real-time)))
    (funcall fn)
    (/ (- (get-internal-real-time) start)
       internal-time-units-per-second)))

(test nursery-sleep-outside-of-a-nursery-sleeps
  (is (>= (elapsed-seconds (lambda () (nursery-sleep 0.1)))
          0.09)))

(test nursery-sleep-sleeps-when-not-cancelled
  (is (>= (elapsed-seconds
           (lambda ()
             (with-nursery (nursery)
               (start-soon nursery (lambda () (nursery-sleep 0.1))))))
          0.09)))

(test cancel-wakes-up-nursery-sleep
  (let ((cleaned-up-p nil)
        (after-sleep-p nil))
    (is (< (elapsed-seconds
            (lambda ()
              (with-nursery (nursery)
                (start-soon nursery
                            (lambda ()
                              (unwind-protect
                                   (progn
                                     (nursery-sleep 60)
                                     (setf after-sleep-p t))
                                (setf cleaned-up-p t))))
                (nursery-sleep 0.1)
                (cancel nursery)
                (nursery-sleep 60))))
           10))
    (is-true cleaned-up-p)
    (is-false after-sleep-p)))

(test failed-task-wakes-up-nursery-sleep
  (let ((failures nil))
    (is (< (elapsed-seconds
            (lambda ()
              (setf failures
                    (failures-of
                     (lambda ()
                       (with-nursery (nursery)
                         (start-soon nursery (lambda ()
                                               (nursery-sleep 0.1)
                                               (error 'my-error))
                                     :name "failing")
                         (nursery-sleep 60)))))))
           10))
    (is (equal '("failing")
               (mapcar #'failure-task-name failures)))))

(test nursery-sleep-in-a-cancelled-nursery-exits-immediately
  (let ((reached-p nil))
    (is (< (elapsed-seconds
            (lambda ()
              (with-nursery (nursery)
                (cancel nursery)
                (nursery-sleep 0)
                (setf reached-p t))))
           10))
    (is-false reached-p)))

(test cancelling-the-outer-nursery-wakes-up-nursery-sleep-in-the-inner-one
  (is (< (elapsed-seconds
          (lambda ()
            (with-nursery (outer)
              (start-soon outer
                          (lambda ()
                            (with-nursery (inner)
                              (start-soon inner (lambda () (nursery-sleep 60)))
                              (nursery-sleep 60))))
              (nursery-sleep 0.1)
              (cancel outer))))
         10)))

(test cancelling-another-nursery-does-not-wake-up-nursery-sleep
  (let ((after-sleep-p nil))
    (with-nursery (outer)
      (start-soon outer
                  (lambda ()
                    (nursery-sleep 0.3)
                    (setf after-sleep-p t)))
      (with-nursery (other)
        (start-soon other (lambda () (nursery-sleep 60)))
        (nursery-sleep 0.1)
        (cancel other)))
    (is-true after-sleep-p)))
