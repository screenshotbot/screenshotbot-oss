;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/webhook/test-webhook
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/webhook/webhook
                #:sign-payload))
(in-package :screenshotbot/webhook/test-webhook)

(util/fiveam:def-suite)

(test sign-payload
  (is (equal "t=1686193117,signature=5d6ec12164ab98b79d4c53e5ede481c2cd283e2035fca7cc6206a4b275e06056"
             (sign-payload "blehbleh"
                           :time
                           (local-time:universal-to-timestamp
                            3895181917)
                           :key "zoidberg"))))
