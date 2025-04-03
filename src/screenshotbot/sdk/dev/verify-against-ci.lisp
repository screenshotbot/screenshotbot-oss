;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev/verify-against-ci
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/dev/record-verify
                #:default-options)
  (:import-from #:clingon.options
                #:make-option))
(in-package :screenshotbot/sdk/dev/verify-against-ci)

(defun verify-against-ci/command ()
  (clingon:make-command
   :name "verify-against-ci"
   :description "Verify your local screenshots against a CI version"
   :options (list*
             (make-option
              :string
              :long-name "threshold"
              :initial-value "0"
              :description "The threshold to use to compare"
              :key :threshold)
             (default-options))
   :handler (lambda (cmd)
              (error "This command is unimplemented"))))


