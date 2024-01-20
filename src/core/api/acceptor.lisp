;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/api/acceptor
  (:use #:cl)
  (:export
   #:api-acceptor-mixin))
(in-package :core/api/acceptor)

(defclass api-acceptor-mixin ()
  ())
