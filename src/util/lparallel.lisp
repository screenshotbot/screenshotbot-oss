;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/lparallel
  (:use #:cl)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:export
   #:immediate-promise))
(in-package :util/lparallel)

(defvar *sleep-time* 1)

(defun immediate-promise (val)
  (let ((promise (lparallel:promise)))
    (lparallel:fulfill promise val)
    promise))
