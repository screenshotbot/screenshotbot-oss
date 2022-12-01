;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-assets
  (:use #:cl
        #:alexandria
        #:fiveam)
  (:import-from #:screenshotbot/assets))
(in-package :screenshotbot/test-assets)

(util/fiveam:def-suite)

(test the-generate-fn-exists
  (is (functionp #'screenshotbot/assets::generate-recorder-platform-assets)))
