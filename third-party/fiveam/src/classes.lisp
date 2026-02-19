;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: FIVEAM; Base: 10; -*-

(in-package :it.bese.fiveam)

(defclass testable-object ()
  ((name :initarg :name :accessor name
         :documentation "A symbol naming this test object.")
   (description :initarg :description :accessor description :initform nil
                :documentation "The textual description of this test object.")
   (depends-on :initarg :depends-on :accessor depends-on :initform nil
               :documentation "The list of AND, OR, NOT forms specifying when to run this test.")
   (status :initarg :status :accessor status :initform :unknown
           :documentation "A symbol specifying the current status
	   of this test. Either: T - this test (and all its
	   dependencies, have passed. NIL - this test
	   failed (either it failed or its dependecies weren't
	   met. :circular this test has a circular dependency
	   and was skipped. Or :depends-not-satisfied or :resolving")
   (profiling-info :accessor profiling-info
                   :initform nil
                   :documentation "An object representing how
                   much time and memory where used by the
                   test.")
   (collect-profiling-info :accessor collect-profiling-info
                           :initarg :collect-profiling-info
                           :initform nil
                           :documentation "When T profiling
                           information will be collected when the
                           test is run.")))

(defmethod print-object ((test testable-object) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (format stream "~S" (name test))))

(defclass test-suite (testable-object)
  ((tests :accessor tests :initform (make-hash-table :test 'eql)
          :documentation "The hash table mapping names to test
	  objects in this suite. The values in this hash table
	  can be either test-cases or other test-suites."))
  (:documentation "A test suite is a collection of tests or test suites.

Test suites serve to organize tests into groups so that the
developer can chose to run some tests and not just one or
all. Like tests test suites have a name and a description.

Test suites, like tests, can be part of other test suites, this
allows the developer to create a hierarchy of tests where sub
trees can be singularly run.

Running a test suite has the effect of running every test (or
suite) in the suite."))

(defclass test-case (testable-object)
  ((test-lambda :initarg :test-lambda :accessor test-lambda
                :documentation "The function to run.")
   (runtime-package :initarg :runtime-package :accessor runtime-package
                    :documentation "By default it stores *package* from the time this test was defined (macroexpanded).")
   (test-suite :initarg :test-suite
               :accessor test-suite
               :initform nil
               :documentation "The test-suite associated with this test"))
  (:documentation "A test case is a single, named, collection of
checks.

A test case is the smallest organizational element which can be
run individually. Every test case has a name, which is a symbol,
a description and a test lambda. The test lambda is a regular
funcall'able function which should use the various checking
macros to collect results.

Every test case is part of a suite, when a suite is not
explicitly specified (either via the :SUITE parameter to the TEST
macro or the global variable *SUITE*) the test is inserted into
the global suite named NIL.

Sometimes we want to run a certain test only if another test has
passed. FiveAM allows us to specify the ways in which one test is
dependent on another.

- AND Run this test only if all the named tests passed.

- OR Run this test if at least one of the named tests passed.

- NOT Run this test only if another test has failed.

FiveAM considers a test to have passed if all the checks executed
were successful, otherwise we consider the test a failure.

When a test is not run due to it's dependencies having failed a
test-skipped result is added to the results."))

(defclass explainer ()
  ())

(defclass text-explainer (explainer)
  ())

(defclass simple-text-explainer (text-explainer)
  ())

(defclass detailed-text-explainer (text-explainer)
  ())

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
