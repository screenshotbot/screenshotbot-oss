;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-sizeof
  (:use #:cl
        #:fiveam)
  (:import-from #:util/sizeof
                #:sizeof))
(in-package :util/tests/test-sizeof)

(util/fiveam:def-suite)

(test simple-sizeof
  (is (eql 8 (sizeof "int64_t"
                     :imports (list "stdint.h")))))

#+(and x86-64 linux)
(test sizeof-with-imports
  (is (eql 8
           (sizeof "fsblkcnt_t"
                   :imports (list "sys/statvfs.h")))))
