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
                #:generate-api-key
                #:cleanup-expired-api-keys
                #:make-transient-key
                #:expired-p
                #:cli-api-key
                #:%find-api-key)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:import-from #:fiveam-matchers/lists
                #:contains))
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

(test cleanup-expired-api-keys
  (with-fixture state ()
    (let ((api-key-1 (make-instance 'cli-api-key))
          (api-key-2 (make-instance 'cli-api-key
                                    :expires-at (- (get-universal-time) 3600)))
          (api-key-3 (make-instance 'cli-api-key
                                    :expires-at (- (get-universal-time) 3800))))
      (cleanup-expired-api-keys)
      (assert-that (bknr.datastore:class-instances 'cli-api-key)
                   (contains api-key-1)))))

(test cleanup-expired-api-keys-for-everything
  (with-fixture state ()
    (let ((api-key-2 (make-instance 'cli-api-key
                                    :expires-at (- (get-universal-time) 3600)))
          (api-key-3 (make-instance 'cli-api-key
                                    :expires-at (- (get-universal-time) 3800))))
      (cleanup-expired-api-keys)
      (assert-that (bknr.datastore:class-instances 'cli-api-key)
                   (contains)))))

(test cli-api-key-that-never-expires
  (with-fixture state ()
    (let ((api-key (make-instance 'cli-api-key :expires-at nil)))
      (assert-that (api-key-key api-key)
                   (matches-regex "cli-.*"))
      (is (eql api-key
               (%find-api-key (api-key-key api-key)))))))

(test we-cant-cheat-our-way-with-api-id
  (with-fixture state ()
    (let ((api-key-old (make-instance 'api-key)))
      (is-false
       (%find-api-key (format nil "cli-~a" (bknr.datastore:store-object-id api-key-old)))))))

(test transient-api-keys-are-never-expired
  (is-false (expired-p (make-transient-key :user :company))))

(test generate-api-key
  (is (equal 20 (length (generate-api-key))))
  (is (not (equal (generate-api-key)
                  (generate-api-key)))))
