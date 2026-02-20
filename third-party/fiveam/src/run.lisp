;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: FIVEAM; Base: 10; -*-

(in-package :it.bese.fiveam)

;;;; * Running Tests

;;;; Once the programmer has defined what the tests are these need to
;;;; be run and the expected effects should be compared with the
;;;; actual effects. FiveAM provides the function RUN for this
;;;; purpose, RUN executes a number of tests and collects the results
;;;; of each individual check into a list which is then
;;;; returned. There are three types of test results: passed, failed
;;;; and skipped, these are represented by TEST-RESULT objects.

;;;; Generally running a test will return normally, but there are two
;;;; exceptional situations which can occur:

;;;; - An exception is signaled while running the test. If the
;;;;   variable *on-error* is :DEBUG than FiveAM will enter the
;;;;   debugger, otherwise a test failure (of type
;;;;   unexpected-test-failure) is returned. When entering the
;;;;   debugger two restarts are made available, one simply reruns the
;;;;   current test and another signals a test-failure and continues
;;;;   with the remaining tests.

;;;; - A circular dependency is detected. An error is signaled and a
;;;;   restart is made available which signals a test-skipped and
;;;;   continues with the remaining tests. This restart also sets the
;;;;   dependency status of the test to nil, so any tests which depend
;;;;   on this one (even if the dependency is not circular) will be
;;;;   skipped.

;;;; The functions RUN!, !, !! and !!! are convenient wrappers around
;;;; RUN and EXPLAIN.

(deftype on-problem-action ()
  '(member :debug :backtrace nil))

(declaim (type on-problem-action *on-error* *on-failure*))

(defvar *on-error* nil
  "The action to perform on error:
- :DEBUG if we should drop into the debugger
- :BACKTRACE to print a backtrace
- NIL to simply continue")

(defvar *on-failure* nil
  "The action to perform on check failure:
- :DEBUG if we should drop into the debugger
- :BACKTRACE to print a backtrace
- NIL to simply continue")

(defvar *debug-on-error* nil
  "T if we should drop into the debugger on error, NIL otherwise.
OBSOLETE: superseded by *ON-ERROR*")

(defvar *debug-on-failure* nil
  "T if we should drop into the debugger on a failing check, NIL otherwise.
OBSOLETE: superseded by *ON-FAILURE*")

(defparameter *print-names* t
  "T if we should print test running progress, NIL otherwise.")

(defparameter *test-dribble-indent* (make-array 0
                                        :element-type 'character
                                        :fill-pointer 0
                                        :adjustable t)
  "Used to indent tests and test suites in their parent suite")

(defun import-testing-symbols (package-designator)
  (import '(5am::is 5am::is-true 5am::is-false 5am::signals 5am::finishes)
          package-designator))

(defparameter *run-queue* '()
  "List of test waiting to be run.")

(define-condition circular-dependency (error)
  ((test-case :initarg :test-case))
  (:report (lambda (cd stream)
             (format stream "A circular dependency wes detected in ~S." (slot-value cd 'test-case))))
  (:documentation "Condition signaled when a circular dependency
between test-cases has been detected."))

(defgeneric run-resolving-dependencies (test)
  (:documentation "Given a dependency spec determine if the spec
is satisfied or not, this will generally involve running other
tests. If the dependency spec can be satisfied the test is also
run."))

(defmethod run-resolving-dependencies ((test test-case))
  "Return true if this test, and its dependencies, are satisfied,
  NIL otherwise."
  (case (status test)
    (:unknown
     (setf (status test) :resolving)
     (if (or (not (depends-on test))
             (eql t (resolve-dependencies (depends-on test))))
         (progn
           (run-test-lambda test)
           (status test))
         (with-run-state (result-list)
           (unless (eql :circular (status test))
             (push (make-instance 'test-skipped
                                  :test-case test
                                  :reason "Dependencies not satisfied")
                   result-list)
             (setf (status test) :depends-not-satisfied)))))
    (:resolving
     (restart-case
         (error 'circular-dependency :test-case test)
       (skip ()
         :report (lambda (s)
                   (format s "Skip the test ~S and all its dependencies." (name test)))
         (with-run-state (result-list)
           (push (make-instance 'test-skipped :reason "Circular dependencies" :test-case test)
                 result-list))
         (setf (status test) :circular))))
    (t (status test))))

(defgeneric resolve-dependencies (depends-on))

(defmethod resolve-dependencies ((depends-on symbol))
  "A test which depends on a symbol is interpreted as `(AND
  ,DEPENDS-ON)."
  (run-resolving-dependencies (get-test depends-on)))

(defmethod resolve-dependencies ((depends-on list))
  "Return true if the dependency spec DEPENDS-ON is satisfied,
  nil otherwise."
  (if (null depends-on)
      t
      (flet ((satisfies-depends-p (test)
               (funcall test (lambda (dep)
                               (eql t (resolve-dependencies dep)))
                        (cdr depends-on))))
        (ecase (car depends-on)
          (and (satisfies-depends-p #'every))
          (or  (satisfies-depends-p #'some))
          (not (satisfies-depends-p #'notany))
          (:before (every #'(lambda (dep)
                              (let ((status (status (get-test dep))))
                                (if (eql :unknown status)
                                    (run-resolving-dependencies (get-test dep))
                                    status)))
                          (cdr depends-on)))))))

(defun results-status (result-list)
  "Given a list of test results (generated while running a test)
  return true if no results are of type TEST-FAILURE.  Returns second
  and third values, which are the set of failed tests and skipped
  tests respectively."
  (let ((failed-tests
          (remove-if-not #'test-failure-p result-list))
        (skipped-tests
          (remove-if-not #'test-skipped-p result-list)))
    (values (null failed-tests)
            failed-tests
            skipped-tests)))

(defun return-result-list (test-lambda)
  "Run the test function TEST-LAMBDA and return a list of all
  test results generated, does not modify the special environment
  variable RESULT-LIST."
  (bind-run-state ((result-list '()))
    (funcall test-lambda)
    result-list))

(defgeneric run-test-lambda (test))

(defmethod run-test-lambda ((test test-case))
  (with-run-state (result-list)
    (bind-run-state ((current-test test))
      (labels ((abort-test (e &optional (reason (format nil "Unexpected Error: ~S~%~A." e e)))
                 (add-result 'unexpected-test-failure
                             :test-expr nil
                             :test-case test
                             :reason reason
                             :condition e))
               (run-it ()
                 (let ((result-list '()))
                   (declare (special result-list))
                   (handler-bind ((check-failure (lambda (e)
                                                   (declare (ignore e))
                                                   (cond
                                                     ((eql *on-failure* :debug)
                                                      nil)
                                                     (t
                                                      (when (eql *on-failure* :backtrace)
                                                        (trivial-backtrace:print-backtrace-to-stream
                                                         *test-dribble*))
                                                      (invoke-restart
                                                       (find-restart 'ignore-failure))))))
                                  (error (lambda (e)
                                           (unless (or (eql *on-error* :debug)
                                                       (typep e 'check-failure))
                                             (when (eql *on-error* :backtrace)
                                               (trivial-backtrace:print-backtrace-to-stream
                                                *test-dribble*))
                                             (abort-test e)
                                             (return-from run-it result-list)))))
                     (restart-case
                         (handler-case
                             (let ((*readtable* (copy-readtable))
                                   (*package* (runtime-package test)))
                               (when *print-names*
                                   (format *test-dribble* "~%~ARunning test ~A " *test-dribble-indent* (name test)))
                               (if (collect-profiling-info test)
                                   ;; Timing info doesn't get collected ATM, we need a portable library
                                   ;; (setf (profiling-info test) (collect-timing (test-lambda test)))
                                   (funcall (test-lambda test))
                                   (funcall (test-lambda test))))
                           (storage-condition (e)
                             ;; heap-exhausted/constrol-stack-exhausted
                             ;; handler-case unwinds the stack (unlike handler-bind)
                             (abort-test e (format nil "STORAGE-CONDITION: aborted for safety. ~S~%~A." e e))
                             (return-from run-it result-list)))
                       (retest ()
                         :report (lambda (stream)
                                   (format stream "~@<Rerun the test ~S~@:>" test))
                         (return-from run-it (run-it)))
                       (ignore ()
                         :report (lambda (stream)
                                   (format stream "~@<Signal an exceptional test failure and abort the test ~S.~@:>" test))
                         (abort-test (make-instance 'test-failure :test-case test
                                                                  :reason "Failure restart."))))
                     result-list))))
        (let ((results (run-it)))
          (setf (status test) (results-status results)
                result-list (nconc result-list results)))))))

(defgeneric %run (test-spec)
  (:documentation "Internal method for running a test. Does not
  update the status of the tests nor the special variables !,
  !!, !!!"))

(defmethod %run ((test test-case))
  (run-resolving-dependencies test))

(defmethod %run ((tests list))
  (mapc #'%run tests))

(defmethod %run ((suite test-suite))
  (when *print-names*
    (format *test-dribble* "~%~ARunning test suite ~A" *test-dribble-indent* (name suite)))
  (let ((suite-results '()))
    (flet ((run-tests ()
             (loop
                for test being the hash-values of (tests suite)
                do (%run test))))
      (vector-push-extend #\space *test-dribble-indent*)
      (unwind-protect
           (bind-run-state ((result-list '()))
             (unwind-protect
                  (if (collect-profiling-info suite)
                      ;; Timing info doesn't get collected ATM, we need a portable library
                      ;; (setf (profiling-info suite) (collect-timing #'run-tests))
                      (run-tests)
                      (run-tests)))
             (setf suite-results result-list
                   (status suite) (every #'test-passed-p suite-results)))
        (vector-pop *test-dribble-indent*)
        (with-run-state (result-list)
          (setf result-list (nconc result-list suite-results)))))))

(defmethod %run ((test-name symbol))
  (when-let (test (get-test test-name))
    (%run test)))

(defvar *initial-!* (lambda () (format t "Haven't run that many tests yet.~%")))

(defvar *!* *initial-!*)
(defvar *!!* *initial-!*)
(defvar *!!!* *initial-!*)

;;;; ** Public entry points

(defun run! (&optional (test-spec *suite*)
             &key ((:print-names *print-names*) *print-names*))
  "Equivalent to (explain! (run TEST-SPEC))."
  (explain! (run test-spec)))

(defun explain! (result-list)
  "Explain the results of RESULT-LIST using a
detailed-text-explainer with output going to *test-dribble*.
Return a boolean indicating whether no tests failed."
  (explain (make-instance 'detailed-text-explainer) result-list *test-dribble*)
  (results-status result-list))

(defun debug! (&optional (test-spec *suite*))
  "Calls (run! test-spec) but enters the debugger if any kind of error happens."
  (let ((*on-error* :debug)
        (*on-failure* :debug))
    (run! test-spec)))

(defun run (test-spec &key ((:print-names *print-names*) *print-names*))
  "Run the test specified by TEST-SPEC.

TEST-SPEC can be either a symbol naming a test or test suite, or
a testable-object object. This function changes the operations
performed by the !, !! and !!! functions."
  (psetf *!* (lambda ()
               (loop :for test :being :the :hash-keys :of *test*
                     :do (setf (status (get-test test)) :unknown))
               (bind-run-state ((result-list '()))
                 (with-simple-restart (explain "Ignore the rest of the tests and explain current results")
                   (%run test-spec))
                 result-list))
         *!!* *!*
         *!!!* *!!*)
  (let ((*on-error*
          (or *on-error* (cond
                           (*debug-on-error*
                            (format *test-dribble* "*DEBUG-ON-ERROR* is obsolete. Use *ON-ERROR*.")
                            :debug)
                           (t nil))))
        (*on-failure*
          (or *on-failure* (cond
                           (*debug-on-failure*
                            (format *test-dribble* "*DEBUG-ON-FAILURE* is obsolete. Use *ON-FAILURE*.")
                            :debug)
                           (t nil)))))
    (funcall *!*)))

(defun ! ()
  "Rerun the most recently run test and explain the results."
  (explain! (funcall *!*)))

(defun !! ()
  "Rerun the second most recently run test and explain the results."
  (explain! (funcall *!!*)))

(defun !!! ()
  "Rerun the third most recently run test and explain the results."
  (explain! (funcall *!!!*)))

(defun run-all-tests (&key (summary :end))
  "Runs all defined test suites, T if all tests passed and NIL otherwise.
SUMMARY can be :END to print a summary at the end, :SUITE to print it
after each suite or NIL to skip explanations."
  (check-type summary (member nil :suite :end))
  (loop :for suite :in (cons 'nil (sort (copy-list *toplevel-suites*) #'string<=))
        :for results := (if (suite-emptyp suite) nil (run suite))
        :when (consp results)
          :collect results :into all-results
        :do (cond
              ((not (eql summary :suite))
               nil)
              (results
               (explain! results))
              (suite
               (format *test-dribble* "Suite ~A is empty~%" suite)))
        :finally (progn
                   (when (eql summary :end)
                     (explain! (alexandria:flatten all-results)))
                   (return (every #'results-status all-results)))))

;; Copyright (c) 2002-2003, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
