;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/model/test-company
    (:use #:cl
          #:fiveam
          #:screenshotbot/model/company)
  (:import-from #:bknr.datastore
                #:delete-object)
  (:import-from #:screenshotbot/user-api
                #:company-name)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/model/company
                #:company-owner
                #:sub-company)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:core/installation/installation
                #:*installation*
                #:installation-domain)
  (:local-nicknames (#:roles #:auth/model/roles)))
(in-package :screenshotbot/model/test-company)

(util/fiveam:def-suite)

(defclass my-installation (multi-org-feature
                           installation)
  ())

(def-fixture state ()
  (with-test-store ()
    (with-installation (:installation (make-instance 'my-installation))
     (&body))))

(test jira-config
  (with-fixture state ()
   (let ((company (make-instance 'company)))
     (is-true (jira-config company))
     (is (eql (jira-config company)
              (jira-config company)))
     (delete-object company))))

(test company-name
  (with-fixture state ()
   (let ((company (make-instance 'company
                                  :personalp t
                                  )))
     (unwind-protect
          (progn
            (is (equal "Default" (company-name
                                  company))))
       (delete-object company)))))

(test sub-company-and-such
  (with-fixture state ()
    (let* ((company (make-instance 'company))
           (child (make-instance 'sub-company
                                :parent company)))
      (let ((user (make-user :companies (list company))))
        (is-true (auth:can-view company user))
        (is-true (auth:can-view child user)))

      (let ((user (make-user :companies (list child))))
        (is-false (auth:can-view company user))
        (is-true (auth:can-view child user))))))


(test can-view-respects-redirect-url
  (with-fixture state ()
    (let* ((company (make-instance 'company))
           (company-with-redirect (make-instance 'company :redirect-url "https://one.example.com"))
           (company-with-redirect-2 (make-instance 'company :redirect-url "https://example.com")))
      (let ((user (make-user :companies (list company-with-redirect company company-with-redirect-2))))
        (is-true (auth:can-view company user))
        (is-false (auth:can-view company-with-redirect user))
        (is-true (auth:can-view company-with-redirect-2 user))
        (let ((*installation* (make-instance 'my-installation :domain "https://one.example.com")))
          (is-true (auth:can-view company-with-redirect user)))))))

(test company-owner
  (with-fixture state ()
    (let* ((user (make-user))
           (company (make-instance 'company)))
      (setf (slot-value company 'screenshotbot/model/company::owner) user)
      (is (eql user (company-owner company))))))

(test company-owner-when-owner-is-role
  (with-fixture state ()
    (let* ((user (make-user))
           (company (make-instance 'company :owner :roles)))
      (make-instance 'roles::user-roles :user user
                                        :company company
                                        :role 'roles:owner)
      (is (eql user (company-owner company))))))

(test company-owner-when-no-owner-exists
  (with-fixture state ()
    (let* ((user (make-user))
           (company (make-instance 'company :owner :roles)))
      (make-instance 'roles::user-roles :user user
                                        :company company
                                        :role 'roles:standard-member)
      (is (eql nil (company-owner company))))))
