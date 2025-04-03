;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/dev/test-verify-against-ci
  (:use #:cl
        #:fiveam))
(in-package :screenshotbot/sdk/dev/test-verify-against-ci)


(util/fiveam:def-suite)

(def-fixture state ()
  (&body))

(test verify-against-ci
  (with-fixture state ()
   (pass)))
