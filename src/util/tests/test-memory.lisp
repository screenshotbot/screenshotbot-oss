;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-memory
  (:use #:cl
        #:fiveam)
  (:import-from #:util/memory
                #:malloc-info
                #:process-mem-usage)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-memory)


(util/fiveam:def-suite)

(test process-mem-usage
  (is (> (process-mem-usage) 0)))

(test malloc-info
  (finishes
    (malloc-info)))
