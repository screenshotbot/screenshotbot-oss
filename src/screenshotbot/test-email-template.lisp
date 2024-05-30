;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-email-template
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:screenshot-test)
  (:import-from #:screenshotbot/email-template
                #:email-template))
(in-package :screenshotbot/test-email-template)

(util/fiveam:def-suite)

(named-readtables:in-readtable markup:syntax)

(screenshot-test basic-email-template-test
  <email-template>
    <p>Hello world!</p>
  </email-template>)
