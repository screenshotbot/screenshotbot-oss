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
                #:active-run
                #:recorder-run)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:screenshotbot/user-api
                #:can-view!
                #:can-view))
(in-package :screenshotbot/dashboard/test-channels)

(util/fiveam:def-suite)

(defclass company-with-channels (test-company)
  ())

(defmethod company-channels ((company company-with-channels))
  (list (make-instance 'test-channel)))

(test simple-view
  (let ((*installation* (make-instance 'installation)))
   (let ((user (make-instance 'test-user))
         (company (make-instance 'company-with-channels)))
     (finishes
      (%list-projects :user user
                      :company company)))))

(def-fixture state ()
  (cl-mock:with-mocks ()
   (with-test-store ()
     (let* ((company (make-instance 'company))
            (channel (find-or-create-channel company "foobar"))
            (run (make-instance 'recorder-run
                                :channel channel
                                :company company))
            (channel-2 (find-or-create-channel company "foobar-2")))
       (setf (active-run channel "master") run)
       (cl-mock:if-called 'can-view!
                          (lambda (x) t))
       (with-fake-request ()
         (auth:with-sessions ()
           (&body)))))))

(test run-for-channel
  (pass)
  (with-fixture state ()
    (assert-that
     (run-for-channel :channel "foobar"
                      :company (oid company)
                      :branch "master")
     (is-not-null))))
