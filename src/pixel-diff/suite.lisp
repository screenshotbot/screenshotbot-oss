;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :pixel-diff/suite
  (:use #:cl))
(in-package :pixel-diff/suite)

(defvar *pixel-diff-suite* (fiveam:def-suite :pixel-diff))

(defun run-pixel-diff-tests ()
  "Run all pixel-diff tests."
  (fiveam:run! :pixel-diff))




