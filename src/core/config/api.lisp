;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/config/api
  (:use #:cl)
  (:export
   #:config
   #:validate
   #:on-config-changed))
(in-package :core/config/api)

(defmethod validate (sym value)
  "Extend this
 to validate the value for a config. The SYM will the
upcased name of the config interned in :KEYWORD"
  nil)

(defgeneric config (key))

(defgeneric (setf config) (value key))

(defgeneric on-config-changed (key value)
  (:documentation "Called any time a config is changed. The key will be the interned
value of the string in the keword package. So 'foo.bar' will become
:FOO.BAR. Use a selector on the key to
select a specific config. This will also be called during store reload.")
  (:method (key value)
    (values)))


