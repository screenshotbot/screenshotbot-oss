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
                #:standard-member
                #:guest
                #:user-role)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that))
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
