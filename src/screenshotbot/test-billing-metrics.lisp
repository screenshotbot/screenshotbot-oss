;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-billing-metrics
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/billing-metrics
                #:incr-billing-metric-impl
                #:incr-billing-metric)
  (:import-from #:core/installation/installation
                #:*installation*))
(in-package :screenshotbot/test-billing-metrics)


(util/fiveam:def-suite)

(test unimpl-metric-happy-path
  (with-installation ()
    (finishes
      (incr-billing-metric :company
                           :my-metric-name
                           20))))

(test impl-metric-happy-path-
  (with-installation ()
    (finishes
      (incr-billing-metric-impl
       *installation*
       :company
       :my-metric-name
       20))))
