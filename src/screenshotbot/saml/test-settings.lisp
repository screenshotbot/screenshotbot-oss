;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/saml/test-settings
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/saml/settings
                #:company-sso-page
                #:sso-page-url)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/store/object-id
                #:oid))
(in-package :screenshotbot/saml/test-settings)

(util/fiveam:def-suite)

(def-fixture state (&key (role 'roles:admin) (enabled t))
  (with-test-store ()
    (with-test-user (:user user :company company :logged-in-p t)
      (setf (roles:user-role company user) role)
      (gk:create :self-service-saml)
      (when enabled
        (gk:allow :self-service-saml company))
      (&body))))

(test non-admin-cannot-see-the-page
  (with-fixture state (:role 'roles:standard-member)
    (signals error
      (company-sso-page :oid (oid company)))))

(test needs-the-gatekeeper
  (with-fixture state (:enabled nil)
    (signals error
      (company-sso-page :oid (oid company)))))

(test cannot-see-another-companys-page
  (with-fixture state ()
    (let ((other (make-instance 'company)))
      (gk:allow :self-service-saml other)
      ;; The handler turns the NO-ACCESS-ERROR into a page
      (let ((html (markup:write-html
                   (company-sso-page :oid (oid other)))))
        (is (str:containsp "You do not have permission" html))
        (is (not (str:containsp "Configure Single-Sign-on" html)))))))

(test admin-sees-their-page
  (with-fixture state ()
    (let ((html (markup:write-html
                 (company-sso-page :oid (oid company)))))
      (is (str:containsp "Configure Single-Sign-on" html)))))

(test page-url
  (with-fixture state ()
    (is (equal (format nil "/company/~a/sso" (oid company))
               (sso-page-url company)))))
