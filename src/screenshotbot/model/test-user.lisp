;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-user
  (:use #:cl
        #:fiveam)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:screenshotbot/user-api
                #:user-email
                #:user
                #:user-companies)
  (:import-from #:screenshotbot/model/company
                #:company-admin-p
                #:get-singleton-company
                #:prepare-singleton-company
                #:personalp
                #:company
                #:company-owner)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature
                #:installation
                #:*installation*)
  (:import-from #:bknr.indices
                #:object-destroyed-p)
  (:import-from #:screenshotbot/model/user
                #:user-email-exists
                #:*lowercase-email-map*
                #:user-with-email
                #:make-user
                #:default-company)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:fiveam-matchers/misc
                #:is-not-null)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:has-typep
                #:assert-that
                #:is-equal-to)
  (:import-from #:auth/model/roles
                #:owner
                #:admin
                #:standard-member
                #:user-role)
  (:import-from #:fiveam-matchers/lists
                #:has-item
                #:contains)
  (:local-nicknames (#:a #:alexandria)
                    (#:roles #:auth/model/roles)))
(in-package :screenshotbot/model/test-user)


(util/fiveam:def-suite)

(defclass pro-installation (installation multi-org-feature)
  ())


(def-fixture state ()
  (with-test-store ()
    (let ((*installation* (make-instance 'pro-installation)))
      (&body))))

(test make-user
  (with-fixture state ()
   (let ((user (make-user)))
     (unwind-protect
          (let ((companies (user-companies user)))
            (is (equal 1 (length companies)))
            (let ((company (car companies)))
              (is-true (personalp company))
              (is-true (roles:has-role-p company user 'roles:admin))))
       (let ((companies (user-companies user)))
         (delete-object user)
         (loop for company in companies
               do (delete-object company)))))))

(test but-with-regular-installation-singleton-company-is-not-deleted
  (with-test-store ()
   (let ((*installation* (make-instance 'installation)))
     (prepare-singleton-company)
     (let* ((user (make-user))
            (companies (user-companies user)))
       (is (equal (list
                   (get-singleton-company *installation*))
                  companies))
       (loop for company in (bknr.datastore:store-objects-with-class 'company)
             do
                (is-false (roles:has-role-p company user 'roles:admin))
                (is-false (roles:has-role-p company user 'roles:owner)))
       (delete-object user)
       (pass)))))

(test default-company
  (with-test-store ()
   (let ((*installation* (make-instance 'pro-installation)))
     (let* ((user (make-user)))
       (is (eql
            (default-company user)
            (car (user-companies user))))))))

(test default-company-for-non-pro
  (with-test-store ()
    (let* ((company (make-instance 'company
                                   :singletonp t))
           (*installation* (make-instance 'installation)))
      (let* ((user (make-user)))
        (is (eql
             (default-company user)
             company))))))

(test default-company-removed-from-user-companies
  (with-fixture state ()
   (let* ((company (make-instance 'company))
          (user-company (make-instance 'company))
          (user (make-user
                 :default-company company
                 :companies (list user-company))))
     (is (eql user-company
              (default-company user))))))

(test user-with-email-is-case-insensitive
  (with-fixture state ()
    (let ((user (make-user :email "IT@example.com")))
      (is (eql user (user-with-email "IT@example.com")))
      (is (eql user (user-with-email "it@example.com"))))))

(test user-with-email-is-case-insensitive-the-other-way-around
  (with-fixture state ()
    (let ((user (make-user :email "it@example.com")))
      (is (eql user (user-with-email "IT@example.com")))
      (is (eql user (user-with-email "it@example.com"))))))

(test user-with-email-is-case-insentivie-even-after-setting-email
  (with-fixture state ()
    (let ((user (make-user :email "foo@example.com")))
      (is (eql user (user-with-email "foo@example.com")))
      (with-transaction ()
        (setf (user-email user) "IT@example.com"))
      (is (equal user (user-with-email "it@example.com")))
      (is (equal nil (user-with-email "foo@example.com"))))))

(test |don't allow me to add a new user with same email|
  (with-fixture state ()
    (make-user :email "IT@example.com")
    (signals user-email-exists
      (make-user :email "it@example.com"))
    ;; check that our store is still valid though
    (make-user :email "foo@example.com")
    (signals user-email-exists
      (make-user :email "IT@example.com"))))

(test simple-find-or-create-user
  (with-fixture state ()
    (assert-that
     (auth:find-or-create-user *installation* :email "foo@example.com")
     (is-not-null))
    (assert-that
     (auth:find-or-create-user *installation* :email "foo@example.com")
     (is-equal-to (first (bknr.datastore:class-instances 'user))))))

(test user-roles-from-old-model
  (with-fixture state ()
    (let* ((company (make-instance 'company))
           (user (make-user :email "foo@example.com"
                            :companies (list company))))
      (assert-that (user-role
                    company user)
                   (has-typep 'standard-member))
      (is-false (company-admin-p company user)))))

(test user-companies-is-updated
  "This test could probably removed in the future. It's a backward compatibility test."
  (with-fixture state ()
    (let* ((company (make-instance 'company))
           (user (make-user :email "foo@example.com")))
      (setf (roles:user-role company user) 'roles:standard-member)
      (assert-that (user-companies user)
                   (has-item company))
      (setf (roles:user-role company user) nil)
      (assert-that (user-companies user)
                   (is-not (has-item company))))))
