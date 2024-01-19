;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-core
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/core
                #:generate-api-key)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-core)


(util/fiveam:def-suite)
