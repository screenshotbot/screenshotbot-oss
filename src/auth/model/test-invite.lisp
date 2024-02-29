;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model/test-invite
  (:use #:cl
        #:fiveam)
  (:import-from #:auth/model/invite
                #:invite-used-p
                #:all-unused-invites
                #:invite-code
                #:invite-with-code
                #:invites-with-email
                #:all-invites
                #:invite)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object))
(in-package :auth/model/test-invite)

(util/fiveam:def-suite)

(defclass my-company (store-object)
  ()
  (:metaclass persistent-class))

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'my-company))
          (company-2 (make-instance 'my-company)))
      (let ((invite (make-instance 'invite :company company
                                           :email "foo@example.com"))
          (invite2 (make-instance 'invite :company company-2
                                          :email "bar@example.com")))
       (&body)))))

(test all-invites ()
  (with-fixture state ()
    (assert-that (all-invites :company company)
                 (contains invite))
    (assert-that (all-invites)
                 (contains invite invite2))))

(test invites-with-email ()
  (with-fixture state ()
    (assert-that (invites-with-email "foo@example.com")
                 (contains invite))))

(test invite-with-code ()
  (with-fixture state ()
    (assert-that (invite-with-code (invite-code invite))
                 (is-equal-to invite))))

(test all-unused-invites ()
  (with-fixture state ()
    (assert-that (all-unused-invites :company company)
                 (contains invite))
    (setf (invite-used-p invite) t)
    (assert-that (all-unused-invites :company company)
                 (contains))))
