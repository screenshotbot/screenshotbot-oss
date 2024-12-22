;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/api/model/test-api-key
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/api-key-api
                #:delete-api-key
                #:api-key-secret-key
                #:api-key-key
                #:api-key)
  (:import-from #:screenshotbot/model/api-key
                #:last-used
                #:flush-last-used-cache
                #:*last-used-cache*
                #:mark-api-key-used
                #:api-key-for-secret
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
                #:contains)
  (:import-from #:util/store/store-version
                #:*snapshot-store-version*
                #:*store-version*)
  (:import-from #:util/store/store-migrations
                #:run-migrations)
  (:import-from #:core/api/model/api-key
                #:api-key-permissions
                #:%permissions)
  (:import-from #:core/installation/installation
                #:abstract-installation
                #:*installation*))
(in-package :core/api/model/test-api-key)


(util/fiveam:def-suite)



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

(test find-api-key-by-secret
  (with-fixture state ()
    (let ((api-key (make-instance 'api-key)))
      (is (eql api-key
               (api-key-for-secret
                (api-key-secret-key api-key)))))))

(test delete-api-key
  (with-fixture state ()
    (let ((api-key (make-instance 'api-key)))
      (finishes
        (delete-api-key api-key))
      (is-false
       (slot-boundp api-key 'api-key))
      (finishes
        (delete-api-key api-key)))))

(test store-migration
  (with-fixture state ()
    (let ((api-key-1 (make-instance 'api-key))
          (api-key-2 (make-instance 'api-key)))
      (delete-api-key api-key-2)
      (let ((*snapshot-store-version* 18)
            (*store-version* 19))
        (run-migrations))
      (is-true (slot-boundp api-key-1 'api-key))
      (is-false (slot-boundp api-key-2 'api-key)))))


(test mark-api-key-used
  (with-fixture state ()
    (let ((api-key (make-instance 'api-key)))
      (mark-api-key-used api-key)
      (mark-api-key-used api-key)
      (is (eql 1 (fset:size *last-used-cache*))))))

(test flush-last-used
  (with-fixture state ()
    (let ((api-key (make-instance 'api-key)))
      (mark-api-key-used api-key)
      (mark-api-key-used api-key)
      (is (eql 1 (fset:size *last-used-cache*)))
      (flush-last-used-cache)
      (is (eql 0 (fset:size *last-used-cache*)))
      (is (> (last-used api-key) 0)))))

(test migration-for-api-key-permissions
  (with-fixture state ()
    (let ((*installation* (make-instance 'abstract-installation))
          (api-key (make-instance 'api-key)))
      (slot-makunbound api-key '%permissions)
      (let ((*store-version* 24)
            (*snapshot-store-version* 23))
        (run-migrations))
      (is (eql nil (api-key-permissions api-key))))))
