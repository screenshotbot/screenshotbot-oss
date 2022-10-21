;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-flags
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/flags
                #:*directory*
                #:*sdk-flags*)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/sdk/test-flags)

(util/fiveam:def-suite)

(test recording-of-flag
  (is (eql '*directory* (gethash :directory *sdk-flags*))))
