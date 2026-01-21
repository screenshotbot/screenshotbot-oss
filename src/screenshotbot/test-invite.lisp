;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-invite
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/testing
                #:with-test-user
                #:screenshot-test
                #:with-installation)
  (:import-from #:screenshotbot/model/invite
                #:invite)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/invite
                #:invite-enabled-p
                #:%user-count
                #:invite-page
                #:user-can-invite-p
                #:invite-post
                #:invite-signup-page
                #:%accept-invite
                #:accept-invite)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:screenshotbot/user-api
                #:unaccepted-invites)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:auth/model/invite
                #:invite-used-p
                #:invite-code)
  (:import-from #:core/installation/auth-provider
                #:auth-providers
                #:default-oidc-provider
                #:company-sso-auth-provider)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:screenshotbot/login/common
                #:standard-auth-provider))
(in-package :screenshotbot/test-invite)

(util/fiveam:def-suite)

(defclass my-installation (multi-org-feature
                           installation)
  ())

(def-fixture state ()
  (with-installation (:installation (make-instance 'my-installation))
   (with-test-store ()
     (let* ((company (make-instance 'company))
            (user (make-user))
            (other-user (make-user :email "bar@example.com")))
       (&body)))))

(test simple-accept-invite
  (with-fixture state ()
    (let ((invite (make-instance 'invite
                                 :inviter user
                                 :company company
                                 :email "bar@example.com")))
      (setf (unaccepted-invites other-user)
            (list invite))
      (assert-that (roles:users-for-company company)
                   (is-not (has-item other-user)))
      (%accept-invite invite other-user)
      (assert-that (roles:users-for-company company)
                   (has-item other-user)))))

(test if-invite-doesnt-exist-we-get-error-page
  (with-fixture state ()
    (with-fake-request ()
      (with-installation ()
        (auth:with-sessions ()
          (assert-that
           (markup:write-html
            (invite-signup-page :invite-code "foo"))
           (contains-string "invite link has expired" )))))))

(screenshot-test invite-expired-page
  (with-fixture state ()
    (with-fake-request ()
      (with-installation ()
        (auth:with-sessions ()
          (invite-signup-page :invite-code "foo"))))))

(test already-a-member
  (with-fixture state ()
    (with-installation ()
      (with-test-user (:user invitee :company company :logged-in-p t)
        (let ((invite (make-instance 'invite
                                     :inviter user
                                     :company company)))
          (catch 'hunchentoot::handler-done
            (invite-signup-page :invite-code (invite-code invite)))
          (assert-that
           (hunchentoot:header-out :location)
           (contains-string
            "/runs")))))))

(screenshot-test inviting-a-user-that-already-exists
  (with-fixture state ()
    (with-installation ()
      (with-test-user (:user user :company company :logged-in-p t)
        (let ((user-2 (make-user :email "car@example.com")))
          (setf (roles:user-role company user-2) 'roles:standard-member)
          (let ((page (invite-post :email "car@example.com")))
            (assert-that
             (markup:write-html
              page)
             (contains-string
              "A user with that email is already a part"))
            page))))))

(screenshot-test no-permission-to-invite
  (with-fixture state ()
    (cl-mock:with-mocks ()
     (with-installation ()
       (with-test-user (:user user :company company :logged-in-p t)
         (cl-mock:answer (user-can-invite-p company user) nil)
         (let ((page (invite-page)))
           (assert-that
            (markup:write-html page)
            (contains-string "You do not have permission"))
           page))))))

(screenshot-test validate-limit-on-number-of-invites
  (with-fixture state ()
    (cl-mock:with-mocks ()
      (with-installation ()
        (with-test-user (:user user :company company :logged-in-p t)
          (roles:ensure-has-role company user
                                 'roles:owner)
          (finishes
            ;; Essentiall test that no checks failed here
            (catch 'hunchentoot::handler-done
              (invite-post :email "foo@example.com")))
          
          (dotimes (i 5)
            (make-instance 'invite
                           :email "dfdfd@gmail.com"
                           :company company))

          (gk:create :limit-invites)
          (gk:allow :limit-invites company)
          
          (let ((page (invite-post :email "foo@example.com")))
            (assert-that
             (markup:write-html page)
             (contains-string "You have reached the limit"))
            page))))))

(screenshot-test if-gk-is-off-then-no-limit-on-invites
  (with-fixture state ()
    (cl-mock:with-mocks ()
      (with-installation ()
        (with-test-user (:user user :company company :logged-in-p t)
          (roles:ensure-has-role company user
                                 'roles:owner)
          (finishes
            ;; Essentiall test that no checks failed here
            (catch 'hunchentoot::handler-done
              (invite-post :email "foo@example.com")))
          
          (dotimes (i 5)
            (make-instance 'invite
                           :email "dfdfd@gmail.com"
                           :company company))

          (finishes
            ;; Essentiall test that no checks failed here
            (catch 'hunchentoot::handler-done
              (invite-post :email "foo@example.com"))))))))

(test user-count-doesnt-count-used-invites
  (with-fixture state ()
    (is (eql 0 (%user-count company)))
    (let ((invites
            (loop for i from 0 to 3
                  collect
                  (make-instance 'invite
                                 :company company
                                 :email "foobar"))))
      (is (eql 4 (%user-count company)))
      (setf (invite-used-p (elt invites 1)) t)
      (is (eql 3 (%user-count company)))
      (setf (invite-used-p (elt invites 2)) t)
      (is (eql 2 (%user-count company))))))

(test invite-enabled-p-happy-path
  (with-fixture state ()
    (is-true (invite-enabled-p company))
    (setf (company-sso-auth-provider company) 'fake)
    (is-false (invite-enabled-p company))))

(test invite-enabled-p-for-installation-with-default-sso
  (with-fixture state ()
    (is-true (invite-enabled-p company))
    (setf (default-oidc-provider *installation*) 'fake)
    (is-false (invite-enabled-p company))))

(test invite-enabled-p-with-company-provider
  (with-fixture state ()
    (is-true company)
    (setf (auth-providers *installation*)
          (list
           (make-instance 'standard-auth-provider
                          :verify-email-p t
                          :company-provider (lambda ()
                                              company))))
    (is-false (invite-enabled-p company))))

(test invite-enabled-p-without-company-provider
  (with-fixture state ()
    (is-true company)
    (setf (auth-providers *installation*)
          (list
           (make-instance 'standard-auth-provider
                          :verify-email-p t
                          :company-provider nil)))
    (is-true (invite-enabled-p company))))

(test invite-enabled-p-with-nil-company-provider
  (with-fixture state ()
    (is-true company)
    (setf (auth-providers *installation*)
          (list
           (make-instance 'standard-auth-provider
                          :verify-email-p t
                          :company-provider (lambda ()
                                              nil))))
    (is-true (invite-enabled-p company))))
