;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev/commands
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/cli-common
                #:dev/command)
  (:import-from #:screenshotbot/sdk/dev/record-verify
                #:verify/command
                #:record/command)
  (:import-from #:screenshotbot/sdk/install
                #:install/command))
(in-package :screenshotbot/sdk/dev/commands)

(defun dev/command ()
  (clingon:make-command
   :name "dev"
   :description "Tools that are run from a developer device (as opposed to CI)"
   :handler (lambda (cmd)
              (clingon:print-usage-and-exit cmd t))
   :sub-commands
   (list
    (record/command)
    (verify/command)
    (install/command))))

