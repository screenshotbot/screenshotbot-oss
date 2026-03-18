;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-billing-meter
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/billing-meter
                #:incr-billing-meter-impl
                #:incr-billing-meter)
  (:import-from #:core/installation/installation
                #:*installation*))
(in-package :screenshotbot/test-billing-meter)


(util/fiveam:def-suite)

(test unimpl-metric-happy-path
  (with-installation ()
    (finishes
      (incr-billing-meter :company
                           :my-metric-name
                           20))))

(test impl-metric-happy-path-
  (with-installation ()
    (finishes
      (incr-billing-meter-impl
       *installation*
       :company
       :my-metric-name
       20))))
