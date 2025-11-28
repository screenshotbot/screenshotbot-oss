;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/mdi
  (:use #:cl)
  (:export
   #:mdi))
(in-package :core/ui/mdi)

(named-readtables:in-readtable markup:syntax)

(markup:deftag mdi (&key name class style)
  <i class= (format nil "material-icons ~a" class) style=style >,(progn name)</i>)
