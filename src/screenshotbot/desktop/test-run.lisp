;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/desktop/test-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/desktop/run
                #:ensure-desktop-api-key)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:with-installation))
(in-package :screenshotbot/desktop/test-run)


(util/fiveam:def-suite)

(test ensure-desktop-api-key
  (with-installation ()
   (with-test-store ()
     (finishes (ensure-desktop-api-key)))))
