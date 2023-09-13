;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(load "scripts/prepare-image")

(ql:quickload :screenshotbot.sdk)

(defvar *output* "screenshotbot-cli")

#+sbcl
(sb-ext:save-lisp-and-die *output*
                          :purify t
                          :toplevel 'screenshotbot/sdk/main:main
                          :executable t)

#+ccl
(ccl:save-application *output*
                      :toplevel-function 'screenshotbot/sdk/main:main)

#+lispworks
(deliver 'screenshotbot/sdk/main:main *output* 5
          :keep-function-name t
          :keep-pretty-printer t
          :keep-lisp-reader t
          :keep-symbols `(system:pipe-exit-status)
          :packages-to-keep-symbol-names :all
          :multiprocessing t)
