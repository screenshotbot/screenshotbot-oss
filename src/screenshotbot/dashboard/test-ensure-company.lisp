;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-ensure-company
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/dashboard/ensure-company
                #:prepare-company
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
                #:user)
  (:import-from #:screenshotbot/installation
                #:call-with-ensure-user-prepared
                #:installation
                #:multi-org-feature
                #:one-owned-company-per-user)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:assert-that))
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
             (is (eql nil (roles:companies-for-user test-user)))
             (let ((ret (ensure-company-or-invite
                         test-user
                         (lambda ()
                           "Should not see this")
                         (list invite))))
               (is (not (equal "Should not see this" ret)))
               ret))))))))

(test call-with-happy-path
  (with-test-store ()
    (with-installation (:installation (make-instance 'my-installation))
      (with-fake-request ()
        (auth:with-sessions ()
         (let ((val 0))
           (is (eql 1
                    (call-with-ensure-user-prepared
                     (make-instance 'my-installation)
                     nil
                     (lambda ()
                       (incf val)))))
           ;; When there is no user, there's nothing to prepare
           (is (eql 1 val))
           (call-with-ensure-user-prepared
            (make-instance 'my-installation)
            (make-instance 'user)
            (lambda ()
              (incf val)))
           ;; The body should not be called in this case because the user
           ;; wasn't prepared
           (is (eql 1 val))))))))

(test prepare-company-happy-path
  (with-test-store ()
    (with-installation (:installation (make-instance 'my-installation))
      (with-fake-request ()
        (auth:with-sessions ()
         (let ((user (make-instance 'user)))
           (setf (auth:current-user) user)
           (finishes
             (prepare-company user "foobar"))
           (assert-that
            (roles:companies-for-user user)
            (has-length 1))))))))
