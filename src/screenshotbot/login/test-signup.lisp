;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-signup
  (:use :cl)
  (:import-from #:auth/model/invite
                #:invite)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:it.bese.fiveam
                #:is-false
                #:is-true
                #:def-fixture
                #:finishes
                #:pass
                #:test
                #:with-fixture)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/login/common
                #:signup-get
                #:standard-auth-provider)
  (:import-from #:screenshotbot/login/github-oauth
                #:github-oauth-provider)
  (:import-from #:screenshotbot/login/signup
                #:valid-email-address-p
                #:confirmation-success
                #:prepare-and-send-email-confirmation
                #:process-existing-invites
                #:render-signup-confirmation
                #:signup-post)
  (:import-from #:screenshotbot/model/company
                #:company
                #:get-singleton-company
                #:prepare-singleton-company)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/testing
                #:screenshot-test
                #:with-installation)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:screenshot-static-page
                #:with-fake-request))
(in-package :screenshotbot/login/test-signup)

(util/fiveam:def-suite)

;; NOTE TO FUTURE SELF: This is a badly written test. This note was
;; added later. This test doesn't run very well in the repl,
;; AFAICT. Works fine on `make test-lw`
(test happy-path
  (with-test-store ()
    (with-fake-request (:host "localhost:80")
      (let ((auth::*iterations* 10))
       (let ((*installation* (make-instance 'installation)))
         (prepare-singleton-company)
         (catch 'hunchentoot::handler-done
           (let* ((company (get-singleton-company *installation*))
                  (auth-provider (make-instance 'standard-auth-provider)))
             (unwind-protect
                  (auth:with-sessions ()
                    (let ((ret (signup-post auth-provider
                                            :email "arnold@tdrhq.com"
                                            :password "foobar23"
                                            :full-name "Arnold Noronha"
                                            :accept-terms-p t
                                            :plan :professional)))
                      (error "should not get here: ~s" ret)))
               (bknr.datastore:delete-object company))))))
      (pass))))

(defclass multi-org-install (multi-org-feature
                             installation)
  ())

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company)))
      (with-installation (:installation (make-instance 'multi-org-install))
        (&body)))))

(def-fixture screenshots (&key (providers (list (make-instance 'standard-auth-provider))))
  (let ((*installation* (make-instance 'installation
                                       :auth-providers providers)))
    (with-fake-request ()
      (auth:with-sessions ()
       (&body)))))

(test screenshot-test
  (with-fixture screenshots ()
    (screenshot-static-page
     :screenshotbot
     "signup-without-oauth"
     (markup:write-html
      (signup-get)))))

(test screenshot-with-oauth
  (with-fixture screenshots (:providers
                             (list
                              (make-instance 'standard-auth-provider)
                              (make-instance 'github-oauth-provider
                                             :client-id "foo"
                                             :client-secret "bar")))
    (screenshot-static-page
     :screenshotbot
     "signup"
     (markup:write-html
      (signup-get)))))

(test screenshot-with-oauth-and-no-login
  (with-fixture screenshots (:providers
                             (list
                              (make-instance 'github-oauth-provider
                                             :client-id "foo"
                                             :client-secret "bar")))
    (screenshot-static-page
     :screenshotbot
     "signup-with-oauth-and-no-login"
     (markup:write-html
      (signup-get)))))

(test signup-error-screen
  (with-fixture screenshots ()
    (screenshot-static-page
     :screenshotbot
     "signup-error-screen"
     (markup:write-html
      (with-form-errors (:errors `((:password . "Incorrect password"))
                         :password "foo"
                         :email "blah@gmail.com"
                         :was-validated t)
        (signup-get))))))

(screenshot-test signup-confirmation-email
  (with-fixture screenshots ()
    (render-signup-confirmation
     "Arnold"
     "dfdsfs23rsfdsf"
     :confirmation-link "https://example.com")))

(screenshot-test email-confirmation-complete
  (with-fixture screenshots ()
    (confirmation-success)))

(test process-existing-invites
  (with-fixture state ()
    (let ((invite (make-instance 'invite :email "foo@example.com"
                                 :company company)))
      (make-instance 'invite :email "bar@example.com"
                     :company company)
      (let ((user (make-user :email "foo@example.com")))
        (process-existing-invites
         user "foo@example.com")
        (assert-that
         (auth:unaccepted-invites user)
         (contains invite))))))

(test process-existing-invites-when-signing-up-with-a-different-email
  (with-fixture state ()
    (let ((invite (make-instance 'invite :email "foo@example.com"
                                         :company (make-instance 'company)))
          (invite-2 (make-instance 'invite :email "bar@example.com"
                                           :company company)))
      (let ((user (make-user :email "foo@example.com")))
        (process-existing-invites
         user "foo@example.com"
         :current-invite invite-2)
        (assert-that
         (auth:unaccepted-invites user)
         (contains invite-2
                   invite))))))

(test prepare-and-send-email-confirmation
  (with-fixture state ()
    (let ((user (make-user :email "foo@example.com"
                           :full-name "Arnold Noronha")))
      (with-fake-request ()
       (finishes
         (prepare-and-send-email-confirmation user))))))

(test valid-email-address-p
  (is-true (valid-email-address-p "arnold@example.com"))
  (is-false (valid-email-address-p "foo@foo@example.com"))
  (is-true (valid-email-address-p "reuxxx.norxxxx@proximite.group")))
