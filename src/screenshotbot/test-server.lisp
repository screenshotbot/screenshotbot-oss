;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-server
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/server)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-server)


(util/fiveam:def-suite)
