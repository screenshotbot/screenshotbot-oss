;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-lparallel
  (:use #:cl
        #:fiveam)
  (:import-from #:util/lparallel
                #:with-temp-lparallel-kernel)
  (:import-from #:lparallel.promise
                #:future)
  (:import-from #:lparallel.promise
                #:force))
(in-package :util/tests/test-lparallel)

(util/fiveam:def-suite)

(test lparallel-reliability-with-only-creation
  (dotimes (i 100) ;; increase this if needed
    (when (= 0 (mod i 100))
      (log:info "count: ~a" i))
    (finishes
      
     (with-temp-lparallel-kernel ()
       (values)))))
