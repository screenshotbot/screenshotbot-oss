;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-disk-size
  (:use #:cl
        #:fiveam)
  (:import-from #:util/disk-size
                #:free-space))
(in-package :util/tests/test-disk-size)

(util/fiveam:def-suite)

(test free-space ()
  (uiop:with-temporary-file (:pathname p)
    (is (> (free-space p) 0))))
