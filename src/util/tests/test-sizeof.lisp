;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-sizeof
  (:use #:cl
        #:fiveam)
  (:import-from #:util/sizeof
                #:def-uint-type
                #:def-int-type
                #:sizeof)
  (:local-nicknames #-lispworks
                    (:fli #:util/fake-fli)))
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

(test def-int-type
  (is (equal
       `(fli:define-c-typedef my-int64-t :int64)
       (macroexpand-1
        (macroexpand-1
         '(def-int-type my-int64-t "int64_t" :imports ("stdint.h"))))))
  (is (equal
       `(fli:define-c-typedef my-uint64-t :uint64)
       (macroexpand-1
        (macroexpand-1
         '(def-uint-type my-uint64-t "uint64_t" :imports ("stdint.h")))))))

(test def-int-type-happy-path
  (finishes
    (def-int-type my-int64-t "int64_t" :imports ("stdint.h")))
  (finishes
   (def-uint-type my-int64-t "uint64_t" :imports ("stdint.h"))))
