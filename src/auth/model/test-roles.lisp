;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model/test-roles
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:auth/model/roles
                #:admin
                #:ensure-has-role
                #:companies-for-user
                #:users-for-company
                #:read-only
                #:reviewer
                #:has-role-p
                #:standard-member
                #:guest
                #:user-role)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains))
(in-package :auth/model/test-roles)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test simple-role-lookup
  (with-fixture state ()
    (setf (user-role :foo :bar) 'guest)
    (assert-that (user-role :foo :bar)
                 (has-typep 'guest))
    (setf (user-role :foo :bar) 'standard-member)
    (assert-that (user-role :foo :bar)
                 (has-typep 'standard-member))))

(test has-role-p
  (with-fixture state ()
    (setf (user-role :foo :bar) 'guest)
    (is-true (has-role-p :foo :bar 'guest))
    (is-false (has-role-p :foo :bar 'reviewer))
    (is-true (has-role-p :foo :bar t))
    (is-false (has-role-p :foo :car t))
    (setf (user-role :foo :bar) 'standard-member)
    (is-true (has-role-p :foo :bar 'read-only))))

(test deleting-a-role
  (with-fixture state ()
    (setf (user-role :foo :bar) 'guest)
    (is-true (has-role-p :foo :bar 'guest))
    (setf (user-role :foo :bar) nil)
    (is-false (has-role-p :foo :bar 'guest))
    ;; Delete a second time!
    (setf (user-role :foo :bar) nil)
    (is-false (has-role-p :foo :bar 'guest))))

(test lookup-by-company-and-index
  (with-fixture state ()
    (setf (user-role :foo :bar) 'guest)
    (assert-that (users-for-company :foo)
                 (contains :bar))
    (assert-that (companies-for-user :bar)
                 (contains :foo))))


(test ensure-a-role
  (with-fixture state ()
    (ensure-has-role :foo :bar 'standard-member)
    (is-true (has-role-p :foo :bar 'standard-member))
    (is-false (has-role-p :foo :bar 'admin))
    (ensure-has-role :foo :bar 'admin)
    (is-true (has-role-p :foo :bar 'standard-member))
    (is-true (has-role-p :foo :bar 'admin))
    (ensure-has-role :foo :bar 'standard-member)
    (is-true (has-role-p :foo :bar 'standard-member))
    (is-true (has-role-p :foo :bar 'admin))))
