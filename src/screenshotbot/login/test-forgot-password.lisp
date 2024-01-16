;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-forgot-password
  (:use :cl)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:with-fixture)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/login/common
                #:standard-auth-provider)
  (:import-from #:screenshotbot/login/forgot-password
                #:change-password-request
                #:forgot-password-page
                #:reset-password-after-confirmation)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/testing
                #:screenshot-test)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/login/test-forgot-password)


(util/fiveam:def-suite)

(def-fixture screenshots (&key (providers (list (make-instance 'standard-auth-provider))))
  (let ((*installation* (make-instance 'installation
                                       :auth-providers providers)))
    (with-fake-request ()
      (auth:with-sessions ()
        (with-test-store ()
          (let ((user (make-user)))
           (&body)))))))


(screenshot-test forgot-password-page ()
  (with-fixture screenshots ()
    (forgot-password-page)))

(screenshot-test reset-password-when-request-is-used-up ()
  (with-fixture screenshots ()
    (reset-password-after-confirmation
     :user user
     :req (make-instance 'change-password-request
                         :used-up-p t))))

(screenshot-test reset-password-when-request-is-active ()
  (with-fixture screenshots ()
    (reset-password-after-confirmation
     :user user
     :req (make-instance 'change-password-request))))
