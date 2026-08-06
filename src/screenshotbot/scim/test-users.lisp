;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/scim/test-users
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/scim/users
                #:scim-delete
                #:scim-get
                #:does-not-exist
                #:uniqueness-error
                #:scim-post)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/misc/lists
                #:only!)
  (:import-from #:screenshotbot/scim/model
                #:scim-user-emails
                #:scim-user)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:screenshotbot/testing
                #:with-test-user)
  (:import-from #:bknr.datastore
                #:class-instances
                #:store-object-id
                #:persistent-class
                #:store-object)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:util/store/object-id
                #:oid))
(in-package :screenshotbot/scim/test-users)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company
                     :logged-in-p t)
     (let ((example-post (uiop:read-file-string
                          ;; Example taken from scim.dev
                          (asdf:system-relative-pathname
                           :screenshotbot
                           "scim/post-example.json"))))
       (&body)))))

(test simple-post
  (with-fixture state ()
    (scim-post
     company
     example-post)
    (let ((user (only! (bknr.datastore:class-instances 'scim-user))))
      (assert-that
       (scim-user-emails user)
       (contains
        "barbara.jensen@example.com")))))

(test uniqueness
  (with-fixture state ()
    (finishes
      (scim-post company example-post))
    (signals uniqueness-error
      (scim-post company example-post))))

(test 404-for-non-existant-id
  (with-fixture state ()
    (signals does-not-exist
      (scim-get company 12323232))))

(defclass fake-object (store-object)
  ()
  (:metaclass persistent-class))

(test 404-for-invalid-object
  (with-fixture state ()
    (let ((id (bknr.datastore:store-object-id (make-instance 'fake-object))))
      (signals does-not-exist
        (scim-get company id)))))

(defun only-id! ()
  (oid (only! (bknr.datastore:class-instances 'scim-user))))

(test 404-for-another-company-user
  (with-fixture state ()
    (let ((other-company (make-instance 'company)))
      (scim-post
       company
       example-post)
      (let ((id (only-id!)))
        (finishes
         (scim-get company id))
        (signals does-not-exist
          (scim-get other-company id))))))

(test delete-happy-path
  (with-fixture state ()
    (scim-post company example-post)
    (finishes
      (scim-delete company (only-id!)))
    (assert-that (class-instances 'scim-user)
                 (has-length 0))))

(test delete-404
  (with-fixture state ()
    (scim-post company example-post)
    (signals does-not-exist
      (scim-delete company 3432424234))))
