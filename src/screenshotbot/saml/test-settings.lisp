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
                #:oid)
  (:import-from #:screenshotbot/saml/settings
                #:redeem-recovery-token
                #:session-recovery-token
                #:sso-principal)
  (:import-from #:screenshotbot/login/recovery-token
                #:make-recovery-token)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/testing
                #:with-fake-request))
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

(def-fixture anonymous ()
  (with-test-store ()
    (with-installation ()
      (with-fake-request ()
        (auth:with-sessions ()
          (let ((company (make-instance 'company)))
            (gk:create :self-service-saml)
            (gk:allow :self-service-saml company)
            (&body)))))))

(defun render-sso-page (company)
  "The rendered SSO page, or NIL if we got something else, e.g. a login
page or a redirect."
  (let ((res (catch 'hunchentoot::handler-done
               (company-sso-page :oid (oid company)))))
    (when res
      (let ((html (markup:write-html res)))
        (when (str:containsp "Configure Single-Sign-on" html)
          html)))))

(test redeeming-a-token-gives-access-to-the-sso-page
  (with-fixture anonymous ()
    (is (eql nil (render-sso-page company)))
    (let ((token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (eql nil (auth:current-user)))
      (is (str:containsp "using a recovery token" (render-sso-page company)))
      (is (typep (sso-principal company)
                 'screenshotbot/login/recovery-token:recovery-token)))))

(test invalid-token-gives-no-access
  (with-fixture anonymous ()
    (let ((html (markup:write-html (redeem-recovery-token "not-a-token"))))
      (is (str:containsp "invalid or has expired" html)))
    (is (eql nil (session-recovery-token company)))
    (is (eql nil (render-sso-page company)))))

(test token-is-only-for-its-company
  (with-fixture anonymous ()
    (let ((other (make-instance 'company))
          (token (nth-value 1 (make-recovery-token company))))
      (gk:allow :self-service-saml other)
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (eql nil (session-recovery-token other)))
      (is (eql nil (sso-principal other)))
      (is (eql nil (render-sso-page other))))))

(test expired-token-gives-no-access
  (with-fixture anonymous ()
    (multiple-value-bind (recovery-token token)
        (make-recovery-token company)
      (signals hex:redirected
        (redeem-recovery-token token))
      (setf (slot-value recovery-token
                        'screenshotbot/login/recovery-token::expires-at)
            (- (get-universal-time) 1))
      (is (eql nil (session-recovery-token company)))
      (is (eql nil (render-sso-page company))))))

(test token-still-needs-the-gatekeeper
  (with-fixture anonymous ()
    (gk:deny :self-service-saml company)
    (let ((token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (signals error
        (company-sso-page :oid (oid company))))))

(test redeeming-a-token-uses-a-new-session
  (with-fixture anonymous ()
    (let ((old-token (auth::%session-token (auth:current-session)))
          (token (nth-value 1 (make-recovery-token company))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (not (equal old-token (auth::%session-token (auth:current-session)))))
      (is (eql nil (auth:session-value :recovery-token
                                       :session (make-instance 'auth::user-session-transient
                                                               :token old-token))))
      (is (not (eql nil (session-recovery-token company)))))))

(test session-access-is-short
  (with-fixture anonymous ()
    (let ((token (nth-value 1 (make-recovery-token company :expires-in (* 24 3600)))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (multiple-value-bind (recovery-token deadline)
          (session-recovery-token company)
        (is (not (eql nil recovery-token)))
        (is (<= deadline (+ (get-universal-time) 3600))))
      ;; The session's access has run out, though the token hasn't
      (destructuring-bind (recovery-token deadline)
          (auth:session-value :recovery-token)
        (declare (ignore deadline))
        (setf (auth:session-value :recovery-token)
              (list recovery-token (- (get-universal-time) 1))))
      (is (eql nil (session-recovery-token company)))
      (is (eql nil (render-sso-page company)))
      ;; Redeeming again gives another hour
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (not (eql nil (session-recovery-token company)))))))

(test session-access-ends-with-the-token
  (with-fixture anonymous ()
    (let ((token (nth-value 1 (make-recovery-token company :expires-in 600))))
      (signals hex:redirected
        (redeem-recovery-token token))
      (is (<= (nth-value 1 (session-recovery-token company))
              (+ (get-universal-time) 600))))))
