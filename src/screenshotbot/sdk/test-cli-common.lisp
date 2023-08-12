;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-cli-common
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/cli-common
                #:root/command))
(in-package :screenshotbot/sdk/test-cli-common)


(util/fiveam:def-suite)


(test can-create-root-command
  (finishes (root/command)))
