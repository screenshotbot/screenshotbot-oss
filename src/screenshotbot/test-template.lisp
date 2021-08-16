;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/test-template
    (:use #:cl
          #:alexandria
          #:./template
          #:fiveam)
  (:import-from #:./factory
                #:*user*
                #:*company*)
  (:import-from #:screenshotbot/template
                #:something-went-wrong)
  (:import-from #:fiveam-matchers
                #:is-string
                #:has-typep
                #:assert-that))

(util/fiveam:def-suite)

(test simple-template
  (screenshotbot/template:dashboard-template
   :user *user*
   :company *company*
   :script-name "/runs")
  (pass))


(test landing-template
  (landing-template
   "foo")
  (pass))

(test something-went-wrong
  (assert-that
   (something-went-wrong)
   (is-string)))
