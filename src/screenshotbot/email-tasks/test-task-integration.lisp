;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/email-tasks/test-task-integration
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/email-tasks/settings
                #:emails-enabledp
                #:email-setting
                #:get-email-settings)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/user-api
                #:channel
                #:current-company
                #:current-user
                #:user)
  (:import-from #:util/testing
                #:screenshot-static-page
                #:with-fake-request)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/model/user
                #:users-for-company
                #:user-personal-company)
  (:import-from #:screenshotbot/login/common
                #:*current-company-override*)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:with-test-user)
  (:import-from #:screenshotbot/email-tasks/task-integration
                #:email-task-integration
                #:send-email-to-user)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/report-api
                #:report)
  (:import-from #:screenshotbot/task-integration-api
                #:send-task)
  (:import-from #:screenshotbot/mailer
                #:send-mail)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/email-tasks/test-task-integration)

(util/fiveam:def-suite)

(defclass multi-installation (installation multi-org-feature)
  ())

(test preconditions
  (with-installation ()
   (with-test-store ()
     (with-fake-request ()
       (auth:with-sessions ()
         (let* ((*installation* (make-instance 'multi-installation)))
           (with-test-user (:company company
                            :user user)
             (setf (current-user) user)
             (setf (current-company) company)
             (screenshot-static-page
              :screenshotbot
              "email-settings-page"
              (markup:write-html
               (get-email-settings)))
             (pass))))))))

(test only-sends-to-enabled-users
  (with-test-store ()
    (with-fake-request ()
      (auth:with-sessions ()
        (let* ((*installation* (make-instance 'multi-installation)))
          (let ((calls nil))
           (cl-mock:with-mocks ()
             (cl-mock:if-called 'send-email-to-user
                                 (lambda (&rest args)
                                   (push args calls)))
             (let* ((company (make-instance 'company :name "foo"))
                    (user1 (make-instance 'user
                                           :companies (list company)))
                    (user2 (make-instance 'user
                                           :companies (list company)))
                    (report (make-instance 'report :company company)))
               (is (equal (list user1 user2)
                          (users-for-company company)))
               (with-transaction ()
                 (setf
                  (emails-enabledp
                   (email-setting :user user1 :company company))
                  nil))
               (send-task (make-instance 'email-task-integration
                                          :company company)
                          report)
               (is (eql 1 (length calls)))
               (is (equal (list user2 company report)
                          (first calls)))))))))))


(test send-email-to-user-happy-path
  (with-test-store ()
    (with-test-user (:user user :company company)
      (let* ((channel (make-instance 'channel
                                     :name "foobar"))
             (run (make-instance 'recorder-run
                                 :channel channel))
             (report (make-instance 'report
                                    :title "1 changes, 2 added"
                                    :run run)))
       (cl-mock:with-mocks ()
         (cl-mock:if-called 'send-mail
                            (lambda (&rest args)))
         (finishes
          (send-email-to-user user company report)))))))
