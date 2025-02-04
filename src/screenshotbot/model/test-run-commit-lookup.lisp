;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-run-commit-lookup
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:screenshotbot/model/run-commit-lookup
                #:*cache*
                #:find-runs-by-commit)
  (:import-from #:fiveam-matchers/lists
                #:contains-in-any-order
                #:contains))
(in-package :screenshotbot/model/test-run-commit-lookup)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (clrhash *cache*)
    (let ((run1 (make-recorder-run
                 :screenshots nil
                 :commit-hash "abcd1234"))
          (run2 (make-recorder-run
                 :screenshots nil
                 :commit-hash "ab1234ab")))
     (&body))))

(test simple-lookup
  (with-fixture state ()
    (assert-that (find-runs-by-commit "abcd")
                 (contains run1))
    (assert-that (find-runs-by-commit "ab")
                 (contains-in-any-order run1 run2))
    (assert-that (find-runs-by-commit "abef")
                 (contains))))

(test caching
  (with-fixture state ()
    (let ((key "abcd"))
      (is (eql
           (find-runs-by-commit key)
           (find-runs-by-commit key))))))
