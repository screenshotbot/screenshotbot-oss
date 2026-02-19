;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: FIVEAM; Base: 10; -*-

(in-package :it.bese.fiveam)

;;;; * Test Suites

;;;; Test suites allow us to collect multiple tests into a single
;;;; object and run them all using asingle name. Test suites do not
;;;; affect the way test are run nor the way the results are handled,
;;;; they are simply a test organizing group.

;;;; Test suites can contain both tests and other test suites. Running
;;;; a test suite causes all of its tests and test suites to be
;;;; run. Suites do not affect test dependencies, running a test suite
;;;; can cause tests which are not in the suite to be run.

;;;; ** Current Suite

(defvar *suite* nil
  "The current test suite object")
;; Only when compiling under ASDF, not under Bazel.
#-bazel
(net.didierverna.asdf-flv:set-file-local-variable *suite*)

;;;; ** Creating Suits

;; Suites that have no parent suites.
(defvar *toplevel-suites* nil)

(defgeneric suite-emptyp (suite)
  (:method ((suite symbol))
    (suite-emptyp (get-test suite)))
  (:method ((suite test-suite))
    (= 0 (hash-table-count (tests suite)))))

(defmacro def-suite (name &key description in)
  "Define a new test-suite named NAME.

IN (a symbol), if provided, causes this suite te be nested in the
suite named by IN. NB: This macro is built on top of make-suite,
as such it, like make-suite, will overrwrite any existing suite
named NAME."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-suite ',name
                 ,@(when description `(:description ,description))
                 ,@(when in `(:in ',in)))
     ',name))

(defmacro def-suite* (name &rest def-suite-args)
  `(progn
     (def-suite ,name ,@def-suite-args)
     (in-suite ,name)))

(defun make-suite (name &key description ((:in parent-suite)))
  "Create a new test suite object.

Overrides any existing suite named NAME."
  (let ((suite (make-instance 'test-suite :name name)))
    (when description
      (setf (description suite) description))
    (when (and name
               (null parent-suite))
      (pushnew name *toplevel-suites*))
    (loop for i in (ensure-list parent-suite)
          for in-suite = (get-test i)
          do (progn
               (when (null in-suite)
                 (cerror "Create a new suite named ~A." "Unknown suite ~A." i)
                 (setf (get-test in-suite) (make-suite i)
                       in-suite (get-test in-suite)))
               (setf (gethash name (tests in-suite)) suite)))
    (setf (get-test name) suite)
    suite))

(eval-when (:load-toplevel :execute)
  (setf *suite*
        (setf (get-test 'nil)
              (make-suite 'nil :description "Global Suite"))))

(defun list-all-suites ()
  "Returns an unordered LIST of all suites."
  (hash-table-values *suite*))

;;;; ** Managing the Current Suite

(defmacro in-suite (suite-name)
  "Set the *suite* special variable so that all tests defined
after the execution of this form are, unless specified otherwise,
in the test-suite named SUITE-NAME.

See also: DEF-SUITE *SUITE*"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%in-suite ,suite-name)))

(defmacro in-suite* (suite-name &key in)
  "Just like in-suite, but silently creates missing suites."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%in-suite ,suite-name :in ,in :fail-on-error nil)))

(defmacro %in-suite (suite-name &key (fail-on-error t) in)
  (with-gensyms (suite)
    `(progn
       (if-let (,suite (get-test ',suite-name))
         (setf *suite* ,suite)
         (progn
           (when ,fail-on-error
             (cerror "Create a new suite named ~A."
                     "Unknown suite ~A." ',suite-name))
           (setf (get-test ',suite-name) (make-suite ',suite-name :in ',in)
                 *suite* (get-test ',suite-name))))
       ',suite-name)))

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
