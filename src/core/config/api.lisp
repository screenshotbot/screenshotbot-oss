;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/config/api
  (:use #:cl)
  (:export
   #:config
   #:validate))
(in-package :core/config/api)

(defmethod validate (sym value)
  "Extend this
 to validate the value for a config. The SYM will the
upcased name of the config interned in :KEYWORD"
  nil)

(defgeneric config (key))

(defgeneric (setf config) (value key))


