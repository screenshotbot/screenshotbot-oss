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
  (:import-from #:screenshotbot/dashboard/api-keys
                #:permission
                #:%read-permissions
                #:api-key-available-permissions)
  (:import-from #:util/store/store
                #:with-test-store))
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

(def-fixture state (&key api-key-roles)
  (with-test-store ()
   (let ((*installation* (make-instance 'my-installation)))
     (gk:create :api-key-roles)
     (funcall (if api-key-roles #'gk:enable #'gk:disable) :api-key-roles)
     (&body))))

(test %read-permissions-without-gk
  (with-fixture state (:api-key-roles nil)
    (is (equal '(:ci) (%read-permissions)))))
