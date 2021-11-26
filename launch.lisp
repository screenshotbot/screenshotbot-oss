;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defun debug-log (e h)
  (declare (ignorable e h))
  (format t "Error somewhere deep in: ~S~%" e)
  (finish-output t)
  #+ccl
  (progn
    (ccl:print-call-history :detailed-p t)
    (ccl:quit)))

#+ (or ccl)
(unless (ccl:getenv "SCREENSHOTBOT_DEBUG")
 (setf *debugger-hook*
       #'debug-log))


(load "scripts/prepare-image")
#+ccl
(ql:quickload "jvm")

#+ccl
(jvm:jvm-init)

(ql:quickload "server")
(ql:quickload "screenshotbot")

(screenshotbot/config:load-config)

(unless (member "compile" (uiop:command-line-arguments) :test 'string=)
  (server:main))

(uiop:quit)
