;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/ui/nibble-span
  (:use #:cl))
(in-package :core/ui/nibble-span)

(named-readtables:in-readtable markup:syntax)

(markup:deftag nibble-span (&key nibble)
  (let ((id (format nil "a~a" (random 1000000000))))
    <markup:merge-tag>
      <span id=id data-nibble-span=nibble class= "nibble-span" >
      </span>
    </markup:merge-tag>))

