;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: FIVEAM; Base: 10; -*-

(in-package :it.bese.fiveam)

;;;; * Analyzing the results

(defparameter *verbose-failures* nil
  "T if we should print the expression failing, NIL otherwise.")

;;;; Just as important as defining and runnig the tests is
;;;; understanding the results. FiveAM provides the function EXPLAIN
;;;; which prints a human readable summary (number passed, number
;;;; failed, what failed and why, etc.) of a list of test results.

(defgeneric explain (explainer results &optional stream recursive-depth)
  (:documentation "Given a list of test results report write to stream detailed
 human readable statistics regarding the results."))

(defmethod explain ((exp detailed-text-explainer) results
                    &optional (stream *test-dribble*) (recursive-depth 0))
  (multiple-value-bind (num-checks passed num-passed passed%
                                   skipped num-skipped skipped%
                                   failed num-failed failed%
                                   unknown num-unknown unknown%)
      (partition-results results)
    (declare (ignore passed))
    (flet ((output (&rest format-args)
             (format stream "~&~vT" recursive-depth)
             (apply #'format stream format-args)))

      (when (zerop num-checks)
        (output "Didn't run anything...huh?")
        (return-from explain nil))
      (output "Did ~D check~P.~%" num-checks num-checks)
      (output "   Pass: ~D (~2D%)~%" num-passed passed%)
      (output "   Skip: ~D (~2D%)~%" num-skipped skipped%)
      (output "   Fail: ~D (~2D%)~%" num-failed failed%)
      (when unknown
        (output "   UNKNOWN RESULTS: ~D (~2D)~%" num-unknown unknown%))
      (terpri stream)
      (when failed
        (output "Failure Details:~%")
        (dolist (f (reverse failed))
          (output "--------------------------------~%")
          (output "~A ~A~@{[~A]~}: ~%"
                  (name (test-case f))
                  (let ((suite (name (test-suite (test-case f)))))
                    (if suite
                        (format nil "in ~A " suite)
                        ""))
                  (description (test-case f)))
          (output "     ~A~%" (reason f))
          (when (for-all-test-failed-p f)
            (output "Results collected with failure data:~%")
            (explain exp (slot-value f 'result-list)
                     stream (+ 4 recursive-depth)))
          (when (and *verbose-failures* (test-expr f))
            (output "    ~S~%" (test-expr f)))
          (output "--------------------------------~%"))
        (terpri stream))
      (when skipped
        (output "Skip Details:~%")
        (dolist (f skipped)
          (output "~A ~@{[~A]~}: ~%"
                  (name (test-case f))
                  (description (test-case f)))
          (output "    ~A~%" (reason f)))
        (terpri stream)))))

(defmethod explain ((exp simple-text-explainer) results
                    &optional (stream *test-dribble*) (recursive-depth 0))
  (multiple-value-bind (num-checks passed num-passed passed%
                                   skipped num-skipped skipped%
                                   failed num-failed failed%
                                   unknown num-unknown unknown%)
      (partition-results results)
    (declare (ignore passed passed% skipped skipped% failed failed% unknown unknown%))
    (format stream "~&~vTRan ~D checks, ~D passed" recursive-depth num-checks num-passed)
    (when (plusp num-skipped)
      (format stream ", ~D skipped " num-skipped))
    (format stream " and ~D failed.~%" num-failed)
    (when (plusp num-unknown)
      (format stream "~vT~D UNKNOWN RESULTS.~%" recursive-depth num-unknown))))

(defun partition-results (results-list)
  (let ((num-checks (length results-list)))
    (destructuring-bind (passed skipped failed unknown)
        (partitionx results-list
                    (lambda (res)
                      (typep res 'test-passed))
                    (lambda (res)
                      (typep res 'test-skipped))
                    (lambda (res)
                      (typep res 'test-failure))
                    t)
      (if (zerop num-checks)
          (values 0
                  nil 0 0
                  nil 0 0
                  nil 0 0
                  nil 0 0)
          (values
           num-checks
           passed (length passed) (floor (* 100 (/ (length passed) num-checks)))
           skipped (length skipped) (floor (* 100 (/ (length skipped) num-checks)))
           failed (length failed) (floor (* 100 (/ (length failed) num-checks)))
           unknown (length unknown) (floor (* 100 (/ (length failed) num-checks))))))))

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
