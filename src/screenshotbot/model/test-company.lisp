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
                #:needs-sso-condition
                #:emails-enabled-by-default-p
                #:ensure-company-using-roles
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
  (:import-from #:auth/viewer-context
                #:normal-viewer-context
                #:logged-in-viewer-context)
  (:import-from #:core/installation/auth-provider
                #:company-sso-auth-provider)
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
      (let* ((user (make-user :companies (list company)))
            (vc (make-instance 'logged-in-viewer-context
                               :user user)))
        (is-true (auth:can-viewer-view vc company))
        (is-true (auth:can-viewer-view vc child)))

      (let* ((user (make-user :companies (list child)))
             (vc (make-instance 'logged-in-viewer-context
                                :user user)))
        (is-false (auth:can-viewer-view vc company))
        (is-true (auth:can-viewer-view vc child))))))


(test can-view-respects-redirect-url
  (with-fixture state ()
    (let* ((company (make-instance 'company))
           (company-with-redirect (make-instance 'company :redirect-url "https://one.example.com"))
           (company-with-redirect-2 (make-instance 'company :redirect-url "https://example.com")))
      (let* ((user (make-user :companies (list company-with-redirect company company-with-redirect-2)))
             (vc (make-instance 'logged-in-viewer-context :user user)))
        (is-true (auth:can-viewer-view vc company))
        (is-false (auth:can-viewer-view vc company-with-redirect))
        (is-true (auth:can-viewer-view vc company-with-redirect-2))
        (let ((*installation* (make-instance 'my-installation :domain "https://one.example.com")))
          (is-true (auth:can-viewer-view vc company-with-redirect)))))))

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

(test make-instance-doesnt-accept-invalid-args
  (with-fixture state ()
    (signals #+lispworks conditions:unknown-keyword-error #-lispworks error
      (make-instance 'company :dfdfdfd 2))))

(test ensure-company-using-roles
  (with-fixture state ()
    (let* ((user (make-user))
           (company (make-instance 'company)))
      (setf (slot-value company 'screenshotbot/model/company::owner) user)
      (ensure-company-using-roles company)
      (is (eql :roles (slot-value company 'screenshotbot/model/company::owner)))
      (is-true
       (roles:has-role-p company user 'roles:owner))

      ;; what if they're already migrated?
      (ensure-company-using-roles company)
      (is-false
       (roles:has-role-p company :roles 'roles:owner)))))


(test emails-enabled-by-default-for-sub-company
  (with-fixture state ()
    (let* ((company (make-instance 'company
                                   :emails-enabled-by-default-p :foobar))
           (sub-company (make-instance 'sub-company
                                       :parent company)))
      (is (eql :foobar (emails-enabled-by-default-p company))))))

(def-fixture user-company-with-sso (&key (role 'roles:admin)
                                    (sso-p t))
  (let ((user (make-user))
        (company (make-instance 'company)))
    (when sso-p
     (setf (company-sso-auth-provider company) :foobar))
    (roles:ensure-has-role company user role)
    (&body)))

(test normal-viewer-context-can-view-sso-company-if-the-user-is-an-admin
  (with-fixture state ()
    (with-fixture user-company-with-sso (:role 'roles:admin)
      (let ((vc (make-instance 'normal-viewer-context :user user)))
        (is-true (auth:can-viewer-view vc company))))))

(test normal-viewer-context-cant-view-sso-company
  (with-fixture state ()
    (with-fixture user-company-with-sso (:role 'roles:standard-member)
      (let ((vc (make-instance 'normal-viewer-context :user user)))
        (is-false (auth:can-viewer-view vc company))))))

(test normal-viewer-context-cant-view-sso-company-and-signals-condition
  (with-fixture state ()
    (with-fixture user-company-with-sso (:role 'roles:standard-member)
      (let ((vc (make-instance 'normal-viewer-context :user user))
            (actual-condition))
        (handler-bind ((needs-sso-condition (lambda (c)
                                              (setf actual-condition c))))
          (auth:can-viewer-view vc company))
        (is-true actual-condition)
        (is (eql company (company actual-condition)))))))

(test normal-viewer-context-can-view-non-sso-company
  (with-fixture state ()
    (with-fixture user-company-with-sso (:role 'roles:standard-member :sso-p nil)
      (let ((vc (make-instance 'normal-viewer-context :user user)))
        (is-true (auth:can-viewer-view vc company))))))
