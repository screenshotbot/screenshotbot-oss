;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/api/dashboard/test-api-keys
  (:use #:cl
        #:fiveam)
  (:import-from #:core/installation/installation
                #:abstract-installation
                #:*installation*)
  (:import-from #:bknr.datastore
                #:persistent-class
                #:store-object)
  (:import-from #:screenshotbot/dashboard/api-keys
                #:%render-api-key
                #:permission
                #:%read-permissions
                #:%can-user-modify
                #:api-key-available-permissions)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/core
                #:does-not
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:screenshotbot/api-key-api
                #:api-key)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:core/api/acceptor
                #:api-token-mode-p)
  (:import-from #:cl-mock
                #:answer))
(in-package :core/api/dashboard/test-api-keys)


;; there are more tests in src/screenshotbot/dashboard/test-api-keys :/

(util/fiveam:def-suite)

(defclass my-installation (abstract-installation)
  ())

(defmethod api-key-available-permissions ((self my-installation))
  (list
   (make-instance
    'permission
    :name :ci
    :default t)
   (make-instance
    'permission
    :name :full)))

(def-fixture state (&key api-key-roles (domain "https://example.com"))
  (cl-mock:with-mocks ()
   (with-test-store ()
     (let ((*installation* (make-instance 'my-installation
                                          :domain domain)))
       (gk:create :api-key-roles)
       (funcall (if api-key-roles #'gk:enable #'gk:disable) :api-key-roles)
       (&body)))))

(test %read-permissions-without-gk
  (with-fixture state (:api-key-roles nil)
    (is (equal '(:ci) (%read-permissions)))))

(defclass fake-company (store-object)
  ()
  (:metaclass persistent-class))

(test %can-user-modify-by-owner-only
  (with-fixture state ()
    (let ((api-key (make-instance 'api-key :user 'user)))
      (is-true (%can-user-modify 'user api-key))
      (is-false (%can-user-modify 'other api-key)))))

(test %can-user-modify-by-admin
  (with-fixture state ()
    (let* ((api-key (make-instance 'api-key :user 'user))
           (company (make-instance 'fake-company)))
      (roles:ensure-has-role company 'user 'roles:admin)
      (is-true (%can-user-modify 'user api-key)))))

(test api-hostname-is-shown-for-enterprise-and-oss-installs
  (with-fixture state (:domain "https://example.com")
    (with-fake-request ()
      (answer (api-token-mode-p *acceptor*) nil)
      (let ((api-key (make-instance 'api-key)))
        (let ((content (%render-api-key api-key)))
          (assert-that (markup:write-html content)
                       (contains-string "SCREENSHOTBOT_API_HOSTNAME")))))))

(test api-hostname-is-not-shown-for-screenshotbot.io
  (with-fixture state (:domain "https://screenshotbot.io")
    (with-fake-request ()
      (answer (api-token-mode-p *acceptor*) nil)
      (let ((api-key (make-instance 'api-key)))
        (let ((content (%render-api-key api-key)))
          (assert-that (markup:write-html content)
                       (does-not
                        (contains-string "example.com")))
          (assert-that (markup:write-html content)
                       (does-not
                        (contains-string "SCREENSHOTBOT_API_HOSTNAME"))))))))
