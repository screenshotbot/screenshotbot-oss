;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop:define-package :screenshotbot/dashboard/test-channels
    (:use #:cl
          #:alexandria
          #:screenshotbot/user-api
          #:fiveam)
  (:import-from #:screenshotbot/dashboard/channels
                #:badge-data
                #:badge-handler
                #:confirm-delete
                #:channel-deleted-confirmation
                #:perform-delete
                #:single-channel-view
                #:run-for-channel
                #:%list-projects)
  (:import-from #:screenshotbot/factory
                #:test-user
                #:test-channel
                #:test-company)
  (:import-from #:screenshotbot/installation
                #:installation
                #:*installation*)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:find-or-create-channel
                #:company)
  (:import-from #:util/object-id
                #:oid)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/misc
                #:is-not-null)
  (:import-from #:screenshotbot/model/recorder-run
                #:runs-for-company
                #:make-recorder-run
                #:active-run
                #:recorder-run)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/user-api
                #:user
                #:can-view!
                #:can-view)
  (:import-from #:screenshotbot/testing
                #:with-installation
                #:fix-timestamps
                #:screenshot-test)
  (:import-from #:cl-mock
                #:if-called
                #:answer)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/dashboard/test-channels)

(util/fiveam:def-suite)

(defclass company-with-channels (company)
  ()
  (:metaclass persistent-class))

(defmethod company-channels ((company company-with-channels))
  (list (make-instance 'test-channel)))

(test simple-view
  (with-test-store ()
   (with-installation ()
     (cl-mock:with-mocks ()
       (let ((user (make-instance 'user))
             (company (make-instance 'company-with-channels :name "Foobar")))
         (answer (auth:can-view! nil))
         (finishes
           (%list-projects :user user
                           :company company)))))))

(def-fixture state ()
  (with-installation ()
   (cl-mock:with-mocks ()
     (with-test-store ()
       (let* ((company (make-instance 'company :name "Dummy company"))
              (channel (find-or-create-channel company "foobar"))
              (user (make-user :companies (list company)))
              (run (make-recorder-run
                    :channel channel
                    :company company))
              (channel-2 (find-or-create-channel company "foobar-2")))
         (setf (active-run channel "master") run)
         (cl-mock:if-called 'can-view!
                            (lambda (x) t))
         (with-fake-request ()
           (auth:with-sessions ()
             (&body))))))))

(test run-for-channel
  (with-fixture state ()
    (assert-that
     (run-for-channel :channel "foobar"
                      :company (oid company)
                      :branch "master")
     (is-not-null))))

(screenshot-test channel-page
  (with-fixture state ()
    (cl-mock:if-called 'oid
                       (lambda (arg)
                         "deadbeef"))
    (fix-timestamps
     (single-channel-view channel))))

(test delete-channel
  (with-fixture state ()
    (perform-delete channel)
    (is (fset:empty? (runs-for-company company)))))

(screenshot-test channel-deleted-confirmation
  (with-fixture state ()
    (channel-deleted-confirmation)))

(screenshot-test confirm-channel-deletion
  (with-fixture state ()
    (confirm-delete channel)))

(screenshot-test list-of-channels
  (with-fixture state ()
    (%list-projects :user user :company company)))

(test badge-handler-happy-path
  (with-fixture state ()
    (if-called
     'badge-data (lambda (&key label message color)
                   message))
    (is
     (equal "0 screenshots"
      (badge-handler :org (oid company)
                     :channel (channel-name channel)
                     :branch "master")))))

(test badge-handler-when-branch-doesnt-exist
  (with-fixture state ()
    (if-called
     'badge-data (lambda (&key label message color)
                   message))
    (is
     (equal "No active run for parameters"
      (badge-handler :org (oid company)
                     :channel (channel-name channel)
                     :branch "fake-branch")))))
