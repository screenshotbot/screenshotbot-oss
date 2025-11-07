;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :screenshotbot/api/test-version
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/api/version
                #:*gk-list*
                #:api-version)
  (:import-from #:screenshotbot/api/model
                #:api-features
                #:installation-url
                #:version-number
                #:*api-version*
                #:version
                #:decode-json)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/api-key-api
                #:api-key)
  (:import-from #:cl-mock
                #:answer)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:fiveam-matchers/core
                #:does-not
                #:assert-that))
(in-package :screenshotbot/api/test-version)

(util/fiveam:def-suite)

(def-fixture state ()
  (cl-mock:with-mocks ()
   (with-test-store ()
     (with-installation ()
       (let* ((company (make-instance 'company))
              (api-key (make-instance 'api-key
                                      :company company
                                      :api-key "foobar"
                                      :api-secret-key "T0pSecret"
                                      :user 'fake-user))
              (*gk-list*
                (append
                 `((:fake-gk nil)
                   (:fake-gk-2 nil)
                   (:fake-gk-3 t))
                 *gk-list*)))
         (gk:create :fake-gk)
         (&body))))))

(test api-version-happy-path
  (with-fixture state ()
    (with-fake-request ()
      (let ((content (api-version)))
        (let ((version (decode-json content 'version)))
          (is (eql *api-version* (version-number version)))
          (is (equal "https://example.com" (installation-url version))))))))

(test features-are-set
  (with-fixture state ()
    (with-fake-request ()
      (gk:allow :fake-gk company)
      (answer (hunchentoot:authorization)
        (values "foobar" "T0pSecret"))
      (let ((content (api-version)))
        (let ((version (decode-json content 'version)))
          (is (eql *api-version* (version-number version)))
          (is (equal "https://example.com" (installation-url version)))
          (assert-that (api-features version)
                       (has-item "fake-gk")
                       (does-not (has-item "fake-gk-2"))))))))

(test default-value-is-respected
  (with-fixture state ()
    (with-fake-request ()
      (gk:allow :fake-gk company)
      (answer (hunchentoot:authorization)
        (values "foobar" "T0pSecret"))
      (let ((content (api-version)))
        (let ((version (decode-json content 'version)))
          (is (eql *api-version* (version-number version)))
          (is (equal "https://example.com" (installation-url version)))
          (assert-that (api-features version)
                       (has-item "fake-gk")
                       (has-item "fake-gk-3")
                       (does-not (has-item "fake-gk-2"))))))))
