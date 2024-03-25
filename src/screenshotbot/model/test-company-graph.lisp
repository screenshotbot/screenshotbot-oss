;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-company-graph
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:multi-org-test-installation
                #:with-installation
                #:with-test-user)
  (:import-from #:screenshotbot/model/company-graph
                #:company-full-graph
                #:company-graph)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:assert-that)
  (:import-from #:bknr.datastore
                #:store-object
                #:persistent-class))
(in-package :screenshotbot/model/test-company-graph)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-installation (:installation (make-instance 'multi-org-test-installation))
     (&body))))

(test happy-path-graph
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (assert-that (company-graph company)
                   (has-item user))
      (with-test-user (:user other-user :company company2 :company-name "other company")
        (assert-that (company-graph company)
                     (is-not (has-item other-user)))))))

(test full-graph-without-connecting-the-companies
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (assert-that (company-full-graph company)
                   (has-item user))
      (with-test-user (:user other-user :company company2 :company-name "other company")
        (assert-that (company-full-graph company)
                     (is-not (has-item other-user)))))))

(defclass dummy-class (store-object)
  ((one :initarg :one)
   (two :initarg :two))
  (:metaclass persistent-class)
  (:documentation "Sometimes you might connect two objects with something like this."))

(test full-graph-finds-everything
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (with-test-user (:user other-user :company company2 :company-name "other company")
        (make-instance 'dummy-class
                       :one user
                       :two other-user)
        (is (not (equal company2 company)))
        (assert-that (company-full-graph company)
                     (has-item other-user)
                     (has-item user)
                     (has-item company2)
                     (has-item company))))))
