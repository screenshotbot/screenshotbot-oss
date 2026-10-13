;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; A level 0 delivery of the CLI, for debugging. Unlike deliver-sdk.lisp
;; nothing is treeshaken, so the binary keeps its debugger, backtraces
;; and symbol names, and it delivers a lot faster. Use `make debug-sdk'
;; to build it. The binary is much larger than the shipping one, so
;; don't hand this to a customer.

(in-package :cl-user)

(ql:quickload :screenshotbot.sdk/debugger)
(ql:quickload :core.cli/deliver)

(defvar *default-output-file* "build/bin/recorder-debug")

(defun output-file ()
  "The destination for the binary, which `make debug-sdk' passes on the
command line."
  (or
   ;; i.e. lispworks -build deliver-debug-sdk.lisp <output-file>
   (third (member "-build" system:*line-arguments-list*
                  :test #'string=))
   *default-output-file*))

(core/cli/deliver:deliver-end-user-cli
 ;; Unlike the shipping CLI, this one starts the LispWorks remote
 ;; debugger if $SCREENSHOTBOT_DEBUG_HOST is set.
 :restart-fn 'screenshotbot/sdk/debugger:main
 :deliver-script *load-truename*
 :output-file (output-file)
 :level 0
 ;; deliver-end-user-cli would default this to NIL at level 0, which
 ;; would leave us without a debugger in the delivered image.
 :keep-debug-mode t
 ;; A universal (fat) binary is pointless for local debugging, and slow
 ;; to build since it delivers once per architecture.
 :universal nil
 :template-builder-fn (lambda ()))
