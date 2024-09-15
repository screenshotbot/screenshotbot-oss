;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-active-run
  (:use #:cl
        #:fiveam)
  (:import-from #:util/hunchentoot-engine
                #:hunchentoot-engine)
  (:import-from #:screenshotbot/server
                #:acceptor)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/sdk/active-run
                #:%get-active-runs)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/api-key-api
                #:api-key-secret-key
                #:api-key-key
                #:api-key)
  (:import-from #:screenshotbot/model/company
                #:find-or-create-channel
                #:get-singleton-company)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:screenshotbot/model/recorder-run
                #:active-run
                #:make-recorder-run)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:screenshotbot/api/model
                #:decode-json)
  (:import-from #:screenshotbot/api/core
                #:*wrap-internal-errors*))
(in-package :screenshotbot/sdk/test-active-run)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
    (with-test-store ()
      (let* ((company (get-singleton-company *installation*))
             (user (make-user :email "foo@gmail.com"))
             (channel (find-or-create-channel company "foobar"))
             (*wrap-internal-errors* nil))
        (roles:ensure-has-role company user 'roles:standard-member)
        (let* ((api-key (make-instance 'api-key
                                       :user user
                                       :company company))
               (acceptor (make-instance 'acceptor))
               (engine (make-instance 'hunchentoot-engine
                                      :acceptor acceptor))
               (api-ctx (make-instance 'api-context
                                       :engine engine
                                       :key (api-key-key api-key)
                                       :secret (api-key-secret-key api-key)
                                       :hostname "example.screenshotbot.io")))
          (&body))))))

(test preconditions
  (with-fixture state ()
    (is (not (str:emptyp (api-key-key api-key))))
    (is (not (str:emptyp (api-key-secret-key api-key))))))

(test find-active-runs
  (with-fixture state ()
    (is
     (eql
      nil
      (%get-active-runs api-ctx :channel "foobar")))))

(test with-an-actual-active-run
  (with-fixture state ()
    (let ((run (make-recorder-run :channel channel)))
      (setf (active-run channel "main")
            run)
      (let ((result (%get-active-runs api-ctx :channel "foobar")))
        (assert-that result
                     (has-length 1))))))
