;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/fset
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro))
(in-package :util/fset)

(def-easy-macro do-reverse-set (&binding var set &fn fn)
  "Loops across a set in the reverse order"
  (loop until (fset:empty? set) do
    (let ((next (fset:greatest set)))
      (fn next)
      (setf set (fset:less set next))))
  nil)
