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
                #:invite-code)
  (:local-nicknames (#:roles #:auth/model/roles)))
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

(screenshot-test already-a-member
  (with-fixture state ()
    (with-installation ()
      (with-test-user (:user invitee :company company :logged-in-p t)
        (let ((invite (make-instance 'invite
                                     :inviter user
                                     :company company)))
          (let ((page (invite-signup-page :invite-code (invite-code invite))))
            (assert-that
             (markup:write-html
              page)
             (contains-string "already a member of this org"))
            page))))))

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
