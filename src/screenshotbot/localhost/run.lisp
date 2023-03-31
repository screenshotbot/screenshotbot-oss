;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/localhost/run
  (:use #:cl)
  (:export
   #:command))
(in-package :screenshotbot/localhost/run)

(defun handler (cmd)
  (log:info "hello"))

(defun command ()
  (clingon:make-command :name "run"
                        :description "Run the screenshotbot service"
                        :handler #'handler))
