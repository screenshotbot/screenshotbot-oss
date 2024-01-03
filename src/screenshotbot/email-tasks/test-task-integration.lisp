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
                #:emails-enabled-by-default-p
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
                #:make-user
                #:users-for-company
                #:user-personal-company)
  (:import-from #:screenshotbot/testing
                #:screenshot-test
                #:with-installation
                #:with-test-user)
  (:import-from #:screenshotbot/email-tasks/task-integration
                #:email-content
                #:users-to-email
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
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/model/channel
                #:channel-subscribers)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/email-tasks/test-task-integration)

(util/fiveam:def-suite)

(defclass multi-installation (installation multi-org-feature)
  ())

(defmethod emails-enabled-by-default-p ((self multi-installation))
  t)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test preconditions
  (with-installation ()
   (with-fixture state ()
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

(defun disable-emails (user1 company)
  (with-transaction ()
    (setf
     (emails-enabledp
      (email-setting :user user1 :company company))
     nil)))

(test only-sends-to-enabled-users
  (with-fixture state ()
    (with-fake-request ()
      (auth:with-sessions ()
        (let* ((*installation* (make-instance 'multi-installation)))
          (let ((calls nil))
           (cl-mock:with-mocks ()
             (cl-mock:if-called 'send-email-to-user
                                 (lambda (&rest args)
                                   (push args calls)))
             (let* ((company (make-instance 'company :name "foo"))
                    (channel (make-instance 'channel :company company))
                    (user1 (make-instance 'user
                                           :companies (list company)))
                    (user2 (make-instance 'user
                                           :companies (list company)))
                    (report (make-instance 'report
                                           :channel channel)))
               (is (equal (list user1 user2)
                          (users-for-company company)))
               (disable-emails user1 company)
               (send-task (make-instance 'email-task-integration
                                          :company company)
                          report)
               (is (eql 1 (length calls)))
               (is (equal (list user2 report)
                          (first calls)))))))))))


(test send-email-to-user-happy-path
  (with-fixture state ()
    (with-fake-request ()
     (with-test-user (:user user :company company)
       (let* ((channel (make-instance 'channel
                                      :name "foobar"))
              (run (make-recorder-run
                    :company company
                    :channel channel))
              (report (make-instance 'report
                                     :title "1 changes, 2 added"
                                     :channel channel
                                     :run run)))
         (cl-mock:with-mocks ()
           (cl-mock:if-called 'send-mail
                              (lambda (&rest args)))
           (finishes
             (send-email-to-user user report))))))))

(test users-to-email-will-include-subscribers
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (let* ((*installation* (make-instance 'multi-installation)))
       (let ((channel (make-instance 'channel :company company)))
         (assert-that (users-to-email channel)
                      (contains user))
         (disable-emails user company)
         (is (equal nil (users-to-email channel)))
         (with-transaction ()
           (push user (channel-subscribers channel)))
         (assert-that (users-to-email channel)
                      (contains user)))))))

(test users-to-email-will-not-include-users-removed-from-company
  (with-fixture state ()
    (let* ((*installation* (make-instance 'multi-installation)))
      (let ((user-2 (make-user)))
        (with-test-user (:user user :company company)
          ;; user-2 is not part of company, probably because they were
          ;; removed from the company
          (let ((channel (make-instance 'channel :company company)))
            (assert-that (users-to-email channel)
                         (contains user))
            (disable-emails user company)
            (is (equal nil (users-to-email channel)))
            (with-transaction ()
              (setf (channel-subscribers channel)
                    (list user user-2)))
            (assert-that (users-to-email channel)
                         (contains user))))))))

(screenshot-test email-for-report-notification
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (let* ((channel (make-instance 'channel
                                     :name "foobar"))
             (run (make-recorder-run
                   :company company
                   :channel channel))
             (report (make-instance 'report
                                    :title "1 changes, 2 added"
                                    :channel channel
                                    :run run)))
        (email-content report)))))
