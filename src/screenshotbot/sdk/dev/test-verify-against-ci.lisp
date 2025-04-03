;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev/test-verify-against-ci
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/integration-fixture
                #:with-sdk-integration)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run))
(in-package :screenshotbot/sdk/dev/test-verify-against-ci)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-sdk-integration (api-context)
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel
                                   :company company
                                   :name "foobar"))
           (run (make-recorder-run
                 :company company
                 :commit-hash "abcd"
                 :channel channel
                 :screenshots nil)))
      (&body))))

(test verify-against-ci
  (with-fixture state ()
   (pass)))
