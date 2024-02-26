;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/active-users/test-active-users
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:core/active-users/active-users
                #:active-user-date
                #:mark-active-user
                #:active-user)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that))
(in-package :core/active-users/test-active-users)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test happy-path
  (with-fixture state ()
    (let ((date (get-universal-time)))
      (mark-active-user :user :foo :company :bleh :date date)
      (assert-that (bknr.datastore:class-instances 'active-user)
                   (has-length 1)))))

(test parses-date-correctly
  (with-fixture state ()
    (let ((date 3917969422 #| picked on the day I implemented this |#))
      (mark-active-user :user :foo :company :bleh :date date)
      (assert-that (active-user-date (car (bknr.datastore:class-instances 'active-user)))
                   (is-equal-to "2024-02-26")))))

(test marks-only-once
  (with-fixture state ()
    (let ((date (get-universal-time)))
      (mark-active-user :user :foo :company :bleh :date date)
      (mark-active-user :user :foo :company :bleh :date date)
      (assert-that (bknr.datastore:class-instances 'active-user)
                   (has-length 1)))))
