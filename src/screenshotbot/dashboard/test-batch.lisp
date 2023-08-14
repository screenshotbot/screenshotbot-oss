;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-batch
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/batch
                #:batch)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/dashboard/batch
                #:render-batch)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/dashboard/test-batch)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-installation ()
     (with-fake-request ()
       (let ((batch (make-instance 'batch)))
         (&body))))))

(screenshot-test batch-item-empty-view
  (with-fixture state ()
    (render-batch batch)))
