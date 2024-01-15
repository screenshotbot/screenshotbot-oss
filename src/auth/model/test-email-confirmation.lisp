;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model/test-email-confirmation
  (:use #:cl
        #:fiveam)
  (:import-from #:it.bese.fiveam
                #:def-fixture)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:auth/model/email-confirmation
                #:confirm-email
                #:user-email-confirmed-p
                #:email-confirmation-code))
(in-package :auth/model/test-email-confirmation)

(util/fiveam:def-suite)

(defclass fake-user (store-object)
  ()
  (:metaclass persistent-class))

(def-fixture state ()
  (with-test-store ()
    (let ((user (make-instance 'fake-user)))
      (&body))))

(test simple-user-confirmed
  (with-fixture state ()
    (is-false (user-email-confirmed-p user))
    (let ((cc (make-instance 'email-confirmation-code
                   :email "foo@example.com"
                   :user user)))
      (is-false (user-email-confirmed-p user))
      (confirm-email cc)
      (is-true (user-email-confirmed-p user)))))
