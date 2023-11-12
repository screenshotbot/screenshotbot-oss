;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-api-key
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/api-key-api
                #:api-key-secret-key
                #:api-key-key
                #:api-key)
  (:import-from #:screenshotbot/model/api-key
                #:make-transient-key
                #:expired-p
                #:cli-api-key
                #:%find-api-key))
(in-package :screenshotbot/model/test-api-key)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))


(test simple-creation
  (with-fixture state ()
    (let ((api-key (make-instance 'api-key)))
      (is (not (str:emptyp (api-key-key api-key))))
      (is (not (str:emptyp (api-key-secret-key api-key)))))))

(test find-api-key
  (with-fixture state ()
    (let ((api-key (make-instance 'api-key)))
      (is (eql api-key
               (%find-api-key (api-key-key api-key)))))))

(test expired-api-key
  (with-fixture state ()
    (let ((api-key (make-instance 'cli-api-key)))
      (is (eql api-key
               (%find-api-key (api-key-key api-key)))))
    (let ((api-key (make-instance 'cli-api-key
                                  :expires-at (- (get-universal-time) 3600))))
      (is (eql nil
               (%find-api-key (api-key-key api-key)))))))

(test cli-api-key-that-never-expires
  (with-fixture state ()
    (let ((api-key (make-instance 'cli-api-key :expires-at nil)))
      (is (eql api-key
               (%find-api-key (api-key-key api-key)))))))

(test transient-api-keys-are-never-expired
  (is-false (expired-p (make-transient-key :user :company))))
