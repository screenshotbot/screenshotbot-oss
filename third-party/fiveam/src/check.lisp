;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: FIVEAM; Base: 10; -*-

(in-package :it.bese.fiveam)

;;;; * Checks

;;;; At the lowest level testing the system requires that certain
;;;; forms be evaluated and that certain post conditions are met: the
;;;; value returned must satisfy a certain predicate, the form must
;;;; (or must not) signal a certain condition, etc. In FiveAM these
;;;; low level operations are called 'checks' and are defined using
;;;; the various checking macros.

;;;; Checks are the basic operators for collecting results. Tests and
;;;; test suites on the other hand allow grouping multiple checks into
;;;; logic collections.

(defvar *test-dribble* t)

(defmacro with-*test-dribble* (stream &body body)
  `(let ((*test-dribble* ,stream))
     (declare (special *test-dribble*))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-special-environment run-state ()
    result-list
    current-test))

;;;; ** Types of test results

;;;; Every check produces a result object.

(defclass test-result ()
  ((reason :accessor reason :initarg :reason :initform "no reason given")
   (test-case :accessor test-case :initarg :test-case)
   (test-expr :accessor test-expr :initarg :test-expr))
  (:documentation "All checking macros will generate an object of
 type TEST-RESULT."))

(defclass test-passed (test-result)
  ()
  (:documentation "Class for successful checks."))

(defgeneric test-passed-p (object)
  (:method ((o t)) nil)
  (:method ((o test-passed)) t))

(define-condition check-failure (error)
  ((reason :accessor reason :initarg :reason :initform "no reason given")
   (test-case :accessor test-case :initarg :test-case)
   (test-expr :accessor test-expr :initarg :test-expr))
  (:documentation "Signaled when a check fails.")
  (:report  (lambda (c stream)
              (format stream "The following check failed: ~S~%~A."
                      (test-expr c)
                      (reason c)))))

(defun process-failure (test-expr &optional reason-format &rest format-args)
  (let ((reason (and reason-format
                     (apply #'format nil reason-format format-args))))
    (with-simple-restart (ignore-failure "Continue the test run.")
      (error 'check-failure :test-expr test-expr
                            :reason reason))
    (add-result 'test-failure :test-expr test-expr
                              :reason reason)))

(defclass test-failure (test-result)
  ()
  (:documentation "Class for unsuccessful checks."))

(defgeneric test-failure-p (object)
  (:method ((o t)) nil)
  (:method ((o test-failure)) t))

(defclass unexpected-test-failure (test-failure)
  ((actual-condition :accessor actual-condition :initarg :condition))
  (:documentation "Represents the result of a test which neither
passed nor failed, but signaled an error we couldn't deal
with.

Note: This is very different than a SIGNALS check which instead
creates a TEST-PASSED or TEST-FAILURE object."))

(defclass test-skipped (test-result)
  ()
  (:documentation "A test which was not run. Usually this is due to
unsatisfied dependencies, but users can decide to skip the test when
appropriate."))

(defgeneric test-skipped-p (object)
  (:method ((o t)) nil)
  (:method ((o test-skipped)) t))

(defun add-result (result-type &rest make-instance-args)
  "Create a TEST-RESULT object of type RESULT-TYPE passing it the
  initialize args MAKE-INSTANCE-ARGS and add the resulting
  object to the list of test results."
  (with-run-state (result-list current-test)
    (let ((result (apply #'make-instance result-type
                         (append make-instance-args (list :test-case current-test)))))
      (etypecase result
        (test-passed  (format *test-dribble* "."))
        (unexpected-test-failure (format *test-dribble* "X"))
        (test-failure (format *test-dribble* "f"))
        (test-skipped (format *test-dribble* "s")))
      (push result result-list))))

;;;; ** The check operators

;;;; *** The IS check

(defmacro is (test &rest reason-args)
  "The DWIM checking operator.

If TEST returns a true value a test-passed result is generated,
otherwise a test-failure result is generated. The reason, unless
REASON-ARGS is provided, is generated based on the form of TEST:

 (predicate expected actual) - Means that we want to check
 whether, according to PREDICATE, the ACTUAL value is
 in fact what we EXPECTED.

 (predicate value) - Means that we want to ensure that VALUE
 satisfies PREDICATE.

 Wrapping the TEST form in a NOT simply produces a negated reason
 string."
  (assert (listp test)
          (test)
          "Argument to IS must be a list, not ~S" test)
  (let (bindings effective-test default-reason-args)
    (with-gensyms (e a v)
      (flet ((process-entry (predicate expected actual &optional negatedp)
               ;; make sure EXPECTED is holding the entry that starts with 'values
               (when (and (consp actual)
                          (eq (car actual) 'values))
                 (assert (not (and (consp expected)
                                   (eq (car expected) 'values))) ()
                                   "Both the expected and actual part is a values expression.")
                 (rotatef expected actual))
               (let ((setf-forms))
                 (if (and (consp expected)
                          (eq (car expected) 'values))
                     (progn
                       (setf expected (copy-list expected))
                       (setf setf-forms (loop for cell = (rest expected) then (cdr cell)
                                              for i from 0
                                              while cell
                                              when (eq (car cell) '*)
                                              collect `(setf (elt ,a ,i) nil)
                                              and do (setf (car cell) nil)))
                       (setf bindings (list (list e `(list ,@(rest expected)))
                                            (list a `(multiple-value-list ,actual)))))
                     (setf bindings (list (list e expected)
                                          (list a actual))))
                 (setf effective-test `(progn
                                         ,@setf-forms
                                         ,(if negatedp
                                              `(not (,predicate ,e ,a))
                                              `(,predicate ,e ,a)))))))
        (list-match-case test
          ((not (?predicate ?expected ?actual))
           (process-entry ?predicate ?expected ?actual t)
           (setf default-reason-args
                 (list "~2&~S~2% evaluated to ~2&~S~2% which is ~2&~S~2%to ~2&~S~2% (it should not be)"
                       `',?actual a `',?predicate e)))
          ((not (?satisfies ?value))
           (setf bindings (list (list v ?value))
                 effective-test `(not (,?satisfies ,v))
                 default-reason-args
                 (list "~2&~S~2% evaluated to ~2&~S~2% which satisfies ~2&~S~2% (it should not)"
                       `',?value v `',?satisfies)))
          ((?predicate ?expected ?actual)
           (process-entry ?predicate ?expected ?actual)
           (setf default-reason-args
                 (list "~2&~S~2% evaluated to ~2&~S~2% which is not ~2&~S~2% to ~2&~S~2%"
                       `',?actual a `',?predicate e)))
          ((?satisfies ?value)
           (setf bindings (list (list v ?value))
                 effective-test `(,?satisfies ,v)
                 default-reason-args
                 (list "~2&~S~2% evaluated to ~2&~S~2% which does not satisfy ~2&~S~2%"
                       `',?value v `',?satisfies)))
          (?_
           (setf bindings '()
                 effective-test test
                 default-reason-args (list "~2&~S~2% was NIL." `',test)))))
      `(let ,bindings
         (if ,effective-test
             (add-result 'test-passed :test-expr ',test)
             (process-failure ',test
                              ,@(or reason-args default-reason-args)))))))

;;;; *** Other checks

(defmacro skip (&rest reason)
  "Generates a TEST-SKIPPED result."
  `(progn
     (format *test-dribble* "s")
     (add-result 'test-skipped :reason (format nil ,@reason))))

(defmacro is-every (predicate &body clauses)
  "The input is either a list of lists, or a list of pairs. Generates (is (,predicate ,expr ,value))
   for each pair of elements or (is (,predicate ,expr ,value) ,@reason) for each list."
  `(progn
     ,@(if (every #'consp clauses)
           (loop for (expected actual . reason) in clauses
                 collect `(is (,predicate ,expected ,actual) ,@reason))
           (progn
             (assert (evenp (list-length clauses)))
             (loop for (expr value) on clauses by #'cddr
                   collect `(is (,predicate ,expr ,value)))))))

(defmacro is-true (condition &rest reason-args)
  "Like IS this check generates a pass if CONDITION returns true
  and a failure if CONDITION returns false. Unlike IS this check
  does not inspect CONDITION to determine how to report the
  failure."
  `(if ,condition
       (add-result 'test-passed :test-expr ',condition)
       (process-failure ',condition
                        ,@(or reason-args
                              `("~S did not return a true value" ',condition)))))

(defmacro is-false (condition &rest reason-args)
  "Generates a pass if CONDITION returns false, generates a
  failure otherwise. Like IS-TRUE, and unlike IS, IS-FALSE does
  not inspect CONDITION to determine what reason to give it case
  of test failure"
  (with-gensyms (value)
    `(let ((,value ,condition))
       (if ,value
           (process-failure ',condition
                            ,@(or reason-args
                                  `("~S returned the value ~S, which is true" ',condition ,value)))
           (add-result 'test-passed :test-expr ',condition)))))

(defmacro signals (condition-spec
                   &body body)
  "Generates a pass if BODY signals a condition of type
CONDITION. BODY is evaluated in a block named NIL, CONDITION is
not evaluated."
  (let ((block-name (gensym)))
    (destructuring-bind (condition &optional reason-control &rest reason-args)
        (ensure-list condition-spec)
      `(block ,block-name
         (handler-bind ((,condition (lambda (c)
                                      (declare (ignore c))
                                      ;; ok, body threw condition
                                      (add-result 'test-passed
                                                  :test-expr ',condition)
                                      (return-from ,block-name t))))
           (block nil
             ,@body))
         (process-failure
           ',condition
           ,@(if reason-control
                 `(,reason-control ,@reason-args)
                 `("Failed to signal a ~S" ',condition)))
         (return-from ,block-name nil)))))

(defmacro finishes (&body body)
  "Generates a pass if BODY executes to normal completion. In
other words if body does signal, return-from or throw this test
fails."
  `(unwind-protect-case () (progn ,@body)
     (:normal (add-result 'test-passed :test-expr ',body))
     (:abort (process-failure ',body "Test didn't finish"))))

(defmacro pass (&rest message-args)
  "Simply generate a PASS."
  `(add-result 'test-passed
               :test-expr ',message-args
               ,@(when message-args
                   `(:reason (format nil ,@message-args)))))

(defmacro fail (&rest message-args)
  "Simply generate a FAIL."
  `(process-failure ',message-args
                    ,@message-args))

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
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
