;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: FIVEAM; Base: 10; -*-

(in-package :it.bese.fiveam)

;;;; ** Fixtures

;;;; When running tests we often need to setup some kind of context
;;;; (create dummy db connections, simulate an http request,
;;;; etc.). Fixtures provide a way to conviently hide this context
;;;; into a macro and allow the test to focus on testing.

;;;; NB: A FiveAM fixture is nothing more than a macro. Since the term
;;;; 'fixture' is so common in testing frameworks we've provided a
;;;; wrapper around defmacro for this purpose.

(defvar *fixture*
  (make-hash-table :test 'eql)
  "Lookup table mapping fixture names to fixture
  objects.")

(defun get-fixture (key &optional default)
  (gethash key *fixture* default))

(defun (setf get-fixture) (value key)
  (setf (gethash key *fixture*) value))

(defun rem-fixture (key)
  (remhash key *fixture*))

(defmacro def-fixture (name (&rest args) &body body)
  "Defines a fixture named NAME. A fixture is very much like a
macro but is used only for simple templating. A fixture created
with DEF-FIXTURE is a macro which can use the special macrolet
&BODY to specify where the body should go.

See Also: WITH-FIXTURE
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get-fixture ',name) (cons ',args ',body))
     ',name))

(defmacro with-fixture (fixture-name (&rest args) &body body)
  "Insert BODY into the fixture named FIXTURE-NAME.

See Also: DEF-FIXTURE"
  (assert (get-fixture fixture-name)
          (fixture-name)
          "Unknown fixture ~S." fixture-name)
  (destructuring-bind ((&rest largs) &rest lbody)
      (get-fixture fixture-name)
    `(macrolet ((&body () '(progn ,@body)))
       (funcall (lambda (,@largs) ,@lbody) ,@args))))

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
