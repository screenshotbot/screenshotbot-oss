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
                #:users-for-company
                #:make-user)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/model/invite
                #:invite)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/invite
                #:%accept-invite
                #:accept-invite)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:screenshotbot/user-api
                #:unaccepted-invites))
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
      (assert-that (users-for-company company)
                   (is-not (has-item other-user)))
      (%accept-invite invite other-user)
      (assert-that (users-for-company company)
                   (has-item other-user)))))
