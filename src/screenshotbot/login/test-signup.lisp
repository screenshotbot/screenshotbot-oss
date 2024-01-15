;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/test-signup
    (:use #:cl
          #:alexandria
          #:fiveam)
  (:import-from #:screenshotbot/login/signup
                #:prepare-and-send-email-confirmation
                #:process-existing-invites
                #:render-signup-confirmation
                #:signup-get
                #:signup-post)
  (:import-from #:screenshotbot/model/company
                #:prepare-singleton-company
                #:get-singleton-company
                #:company)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature
                #:standard-auth-provider
                #:*installation*
                #:installation)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:screenshot-static-page
                #:with-fake-request)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/login/github-oauth
                #:github-oauth-provider)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/model/invite
                #:invite)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains))

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
           (let* ((company (get-singleton-company *installation*)))
             (unwind-protect
                  (auth:with-sessions ()
                    (let ((ret (signup-post  :email "arnold@tdrhq.com"
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
