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
                #:user
                #:user-companies)
  (:import-from #:screenshotbot/model/company
                #:get-singleton-company
                #:prepare-singleton-company
                #:personalp
                #:company
                #:company-admins
                #:company-owner)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature
                #:installation
                #:*installation*)
  (:import-from #:bknr.indices
                #:object-destroyed-p)
  (:import-from #:screenshotbot/model/user
                #:make-user
                #:default-company)
  (:import-from #:util/store
                #:with-test-store)
  (:local-nicknames (#:a #:alexandria)))
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
              (is (equal (list user)
                         (company-admins company))))
            (pass))
       (let ((companies (user-companies user)))
         (delete-object user)
         (loop for company in companies
               do (delete-object company)))))))

(test remove-reference-from-companies-for-testing
  (with-fixture state ()
   (let ((user (make-user)))
     (let ((company (car (user-companies user))))
       (unwind-protect
            (is-true (company-owner company))
         (delete-object user))

       (unwind-protect
            (progn
              (is-false (company-owner company)))
         (delete-object company))))))


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
                (is (not (member user (ignore-errors (company-admins company)))))
                (is (not (eql user (ignore-errors (company-owner company))))))
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
   (let* ((company (make-instance 'company))
          (*installation* (make-instance 'installation
                                          :singleton-company company)))
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
