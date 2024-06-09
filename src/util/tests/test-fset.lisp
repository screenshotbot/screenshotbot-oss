;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-fset
  (:use #:cl
        #:fiveam)
  (:import-from #:serapeum
                #:collecting)
  (:import-from #:util/fset
                #:do-reverse-set)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains))
(in-package :util/tests/test-fset)

(util/fiveam:def-suite)

(test in-reverse-order
  (is (eql :greater (fset:compare "three" "two")))
  (let ((set (fset:convert 'fset:set (list 1 3 2))))
    (assert-that
     (collecting
       (do-reverse-set (var set)
         (collect var)))
     (contains
      3 2 1))))

(test return-from-block
  (let ((set (fset:convert 'fset:set (list 1 3 4 2 5))))
    (is (eql
         4
         (do-reverse-set (var set)
           (when (= 0 (mod var 2))
             (return var)))))))
