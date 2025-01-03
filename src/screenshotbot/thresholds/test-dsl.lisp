;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/thresholds/test-dsl
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/thresholds/dsl
                #:%read-dsl-from-string
                #:%read-dsl)
  (:local-nicknames
   (#:dsl #:screenshotbot/thresholds/dsl)))
(in-package :screenshotbot/thresholds/test-dsl)

(util/fiveam:def-suite)

(test read-exact
  (is
   (equal
    '(dsl:exact-images)
    (%read-dsl-from-string "(exact-images)"))))
