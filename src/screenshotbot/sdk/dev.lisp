;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev
  (:use #:cl)
  (:export
   #:dev/command))
(in-package :screenshotbot/sdk/dev)

(defun record/command ()
  (clingon:make-command
   :name "record"
   :description "Record a user run"
   :handler (lambda (cmd)
              (error "unimplemented"))))

(defun verify/command ()
  (clingon:make-command
   :name "verify"
   :description "Verify a run against the last recorded run"
   :handler (lambda (cmd)
              (error "unimplemented"))))

(defun dev/command ()
  (clingon:make-command
   :name "dev"
   :description "Tools that are run from a developer device (as opposed to CI)"
   :sub-commands
   (list
    (record/command)
    (verify/command))))
