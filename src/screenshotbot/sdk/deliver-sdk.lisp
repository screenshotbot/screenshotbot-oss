;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package :cl-user)

(ql:quickload :screenshotbot.sdk)

(defun deliver-main ()
 (let ((output-file (cadddr system:*line-arguments-list*)))
   (delete-file output-file)
   (deliver 'screenshotbot/sdk/main:main output-file 5
            :keep-function-name t
            :keep-pretty-printer t
            :keep-lisp-reader t
            ;; temporary: get the build green
            :keep-eval t
            :keep-symbols `(system:pipe-exit-status)
            :packages-to-keep-symbol-names :all
            :multiprocessing t)))

(compile 'deliver-main)

(deliver-main)
(uiop:quit)
