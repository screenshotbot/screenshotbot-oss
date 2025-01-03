;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/thresholds/dsl
  (:use #:cl)
  (:export #:exact-images))
(in-package :screenshotbot/thresholds/dsl)


(defun %read-dsl-from-string (code)
  (let ((input (make-string-input-stream code)))
    (let ((*package* (symbol-package 'foo)))
     (%read-dsl input))))

(defun %read-dsl (stream)
  (read stream))



