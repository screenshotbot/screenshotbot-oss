;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-compare-branches
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/dashboard/compare-branches
                #:%form)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/dashboard/test-compare-branches)


(util/fiveam:def-suite)


(def-fixture state ()
  (with-installation ()
   (with-fake-request ()
     (auth:with-sessions ()
       (&body)))))

(screenshot-test compare-branches-form
  (with-fixture state ()
    (%form)))
