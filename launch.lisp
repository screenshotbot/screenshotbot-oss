;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(load "scripts/prepare-image")
(load "scripts/init")

#+ccl
(ql:quickload "jvm")

#+ccl
(jvm:jvm-init)

(ql:quickload "server")
(ql:quickload "server/slynk")
(ql:quickload "screenshotbot/all")

(screenshotbot/config:load-config)

(unless (member "compile" (uiop:command-line-arguments) :test 'string=)
  (server:main))

(uiop:quit)
