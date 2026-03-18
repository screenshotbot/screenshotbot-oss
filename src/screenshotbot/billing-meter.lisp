;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/billing-meter
  (:use #:cl)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:util/threading
                #:ignore-and-log-errors))
(in-package :screenshotbot/billing-meter)

(defgeneric incr-billing-meter-impl (installation company name value)
  (:documentation "Increment a billing metric for the given company.

name is a symbol for the metric name.

value is a number.

By default this does nothing.")
  (:method (installation company name value)
    nil))

(defmethod incr-billing-meter (company name value)
  (ignore-and-log-errors ()
   (incr-billing-meter-impl
    *installation*
    company
    name
    value)))

