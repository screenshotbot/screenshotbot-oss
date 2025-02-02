;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/compare-branches
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/template
                #:app-template))
(in-package :screenshotbot/dashboard/compare-branches)

(named-readtables:in-readtable markup:syntax)

(defhandler (nil :uri "/compare-branches") ()
  (assert (gk:check :compare-branches (auth:current-company)))
  <app-template>
    hello world
  </app-template>)


