;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/fset
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:do-reverse-set))
(in-package :util/fset)

(defun call-do-reverse-set (set fn)
  (loop until (fset:empty? set) do
    (let ((next (fset:greatest set)))
      (funcall fn next)
      (setf set (fset:less set next))))
  nil)

(defmacro do-reverse-set ((val set) &body body)
  "Loops across a set in the reverse order. Wrapped in an implicit NIL
block."
  `(block nil
     (call-do-reverse-set
      ,set
      (lambda (,val)
        ,@body))))
