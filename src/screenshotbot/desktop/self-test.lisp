;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/desktop/self-test
  (:use #:cl)
  (:import-from #:util/health-check
                #:run-health-checks)
  (:export
   #:command))
(in-package :screenshotbot/desktop/self-test)

(defun handler (cmd)
  (declare (ignore cmd))
  (run-health-checks))

(defun command ()
  (clingon:make-command :name "self-test"
                        :description "Run some sanity self tests on this binary and installation"
                        :handler #'handler))
