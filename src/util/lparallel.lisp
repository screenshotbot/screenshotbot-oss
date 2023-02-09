;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/lparallel
  (:use #:cl)
  (:export
   #:bind-future))
(in-package :util/lparallel)

(defun bind-future (promise fn)
  (lparallel:future
    (let ((res (lparallel:force promise)))
      (funcall fn res))))
