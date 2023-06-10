;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webhook/test-model
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/webhook/model
                #:signing-key
                #:ensure-webhook-config)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:fiveam-matchers/misc
                #:is-not-null)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:is-not-empty))
(in-package :screenshotbot/webhook/test-model)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company)))
      (&body))))

(test ensure-webhook-config
  (with-fixture state ()
    (let ((config (ensure-webhook-config company)))
      (assert-that config
                   (is-not-null))
      (is (eql (ensure-webhook-config company)
               config))
      (assert-that (signing-key config)
                   (is-not-empty)))))
