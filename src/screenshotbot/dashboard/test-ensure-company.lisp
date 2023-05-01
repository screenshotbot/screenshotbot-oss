;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-ensure-company
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/ensure-company
                #:ensure-company-or-invite
                #:%new-company)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:screenshot-test)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/model/invite
                #:invite)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/user-api
                #:user-companies
                #:user)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature
                #:one-owned-company-per-user))
(in-package :screenshotbot/dashboard/test-ensure-company)


(util/fiveam:def-suite)

(screenshot-test empty-new-company-for-companyless-user
  (with-installation ()
    (with-fake-request ()
      (auth:with-sessions ()
        (%new-company)))))

(defclass my-installation (one-owned-company-per-user
                           multi-org-feature
                           installation)
  ())

(screenshot-test ensure-invite-too
  (with-installation (:installation (make-instance 'my-installation))
    (with-fake-request ()
      (with-test-store ()
       (auth:with-sessions ()
         (let* ((company (make-instance 'company
                                        :name "FooBar Enterprises"))
                (user (make-instance 'user
                                     :full-name "Arnold Noronha"))
                (invite (make-instance 'invite
                                       :inviter user
                                       :company company)))
           (let ((test-user (make-instance 'user :full-name "Another user")))
             (is (eql nil (user-companies test-user)))
             (let ((ret (ensure-company-or-invite
                         test-user
                         (lambda ()
                           "Should not see this")
                         (list invite))))
               (is (not (equal "Should not see this" ret)))
               ret))))))))
