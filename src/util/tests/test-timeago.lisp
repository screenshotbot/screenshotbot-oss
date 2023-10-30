;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-timeago
  (:use #:cl
        #:fiveam)
  (:import-from #:util/timeago
                #:timeago))
(in-package :util/tests/test-timeago)

(util/fiveam:def-suite)

(test timeago-works-on-nil
  (is (equal nil
             (timeago :timestamp nil))))
