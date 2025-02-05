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
                #:contains)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:channel))
(in-package :screenshotbot/model/test-run-commit-lookup)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (clrhash *cache*)
    (let* ((company (make-instance 'company))
           (channel (make-instance 'channel
                                   :github-repo "foo"))
           (run1 (make-recorder-run
                  :screenshots nil
                  :channel channel
                  :commit-hash "abcd1234"))
           (run2 (make-recorder-run
                  :screenshots nil
                  :channel channel
                  :commit-hash "ab1234ab")))
      (&body))))

(test simple-lookup
  (with-fixture state ()
    (assert-that (find-runs-by-commit "abcd" :company :all)
                 (contains run1))
    (assert-that (find-runs-by-commit "ab" :company :all)
                 (contains-in-any-order run1 run2))
    (assert-that (find-runs-by-commit "abef" :company :all)
                 (contains))))

(test caching
  (with-fixture state ()
    (let ((key "abcd"))
      (is (eql
           (find-runs-by-commit key :company :all)
           (find-runs-by-commit key :company :all))))))

(test lookup-by-company-being-nil
  (with-fixture state ()
    (assert-that (find-runs-by-commit "abcd" :company nil
                                             :repo "foo")
                 (contains))))

(test lookup-by-specific-company
  (with-fixture state ()
    (let ((run3 (make-recorder-run
                 :screenshots nil
                 :company company
                 :channel channel
                 :commit-hash "abcd1234")))
      (assert-that (find-runs-by-commit "abcd" :company company :repo "foo")
                   (contains run3))
      (assert-that (find-runs-by-commit "abcd" :company :all)
                   (contains-in-any-order run1 run3)))))

(test filters-by-right-channel
  (with-fixture state ()
    (let* ((channel-2 (make-instance 'channel
                                     :github-repo "bar"))
           (run3 (make-recorder-run
                  :screenshots nil
                  :company company
                  :channel channel
                  :commit-hash "abcd1234"))
           (run4 (make-recorder-run
                  :screenshots nil
                  :company company
                  :channel channel-2
                  :commit-hash "abcd1234")))
      (assert-that (find-runs-by-commit "abcd" :company company :repo "foo")
                   (contains run3))
      (assert-that (find-runs-by-commit "abcd" :company company :repo "bar")
                   (contains run4)))))

