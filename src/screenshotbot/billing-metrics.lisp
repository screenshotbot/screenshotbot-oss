;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/billing-metrics
  (:use #:cl)
  (:import-from #:core/installation/installation
                #:*installation*))
(in-package :screenshotbot/billing-metrics)

(defgeneric incr-billing-metric-impl (installation company name value)
  (:documentation "Increment a billing metric for the given company.

name is a symbol for the metric name.

value is a number.

By default this does nothing.")
  (:method (installation company name value)
    nil))

(defmethod incr-billing-metric (company name value)
  (incr-billing-metric-impl
   *installation*
   company
   name
   value))

