;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/figma/test-view
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/dashboard/compare
                #:associate-figma)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/figma/test-view)


(util/fiveam:def-suite)

(screenshot-test associate-figma-form
  (with-installation ()
   (with-fake-request ()
     (associate-figma))))



