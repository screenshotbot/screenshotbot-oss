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
                #:invalid-email
                #:only-one-email
                #:user-name-must-be-email
                #:validate-dto
                #:%list-users
                #:scim-get
                #:does-not-exist
                #:uniqueness-error
                #:scim-post)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:util/misc/lists
                #:only!)
  (:import-from #:screenshotbot/scim/model
                #:scim-users-for-company
                #:scim-user-activep
                #:scim-user-emails
                #:scim-user-v2)
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
                #:oid)
  (:import-from #:screenshotbot/api/model
                #:encode-json)
  (:import-from #:screenshotbot/scim/dto
                #:external-email
                #:external-email-value
                #:external-user-emails
                #:list-response-resources
                #:external-user-activep
                #:external-user)
  (:import-from #:screenshotbot/user-api
                #:user))
(in-package :screenshotbot/scim/test-users)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company
                     :user user
                     :logged-in-p t)
      (setf (roles:user-role company user) nil) ;; to keep old tests passing
      (let ((example-post (uiop:read-file-string
                           ;; Example taken from scim.dev
                           (asdf:system-relative-pathname
                            :screenshotbot
                            "scim/post-example.json"))))
        (&body)))))

(test simple-post
  (with-fixture state ()
    (assert-that
     (class-instances 'user)
     (has-length 1))    
    (scim-post
     company
     example-post)
    (assert-that
     (class-instances 'user)
     (has-length 2))
    (let ((user (only! (scim-users-for-company company))))
      (assert-that
       (scim-user-emails user)
       (contains
        "barbara.jensen@example.com")))))

(test simple-list-users
  (with-fixture state ()
    (scim-post
     company
     example-post)
    (let ((list-response (%list-users company nil)))
     (let ((user (only! (list-response-resources list-response))))
       (assert-that
        (mapcar #'external-email-value (external-user-emails user))
        (contains
         "barbara.jensen@example.com"))))))

(test list-users-with-a-basic-filter
  (with-fixture state ()
    (scim-post
     company
     example-post)
    (let ((list-response (%list-users company "userName eq \"barbara.jensen@example.com\"")))
     (let ((user (only! (list-response-resources list-response))))
       (assert-that
        (mapcar #'external-email-value (external-user-emails user))
        (contains
         "barbara.jensen@example.com"))))))

(test list-users-with-a-basic-filter-that-does-not-match
  (with-fixture state ()
    (scim-post
     company
     example-post)
    (let ((list-response (%list-users company "userName eq \"carbar@example.com\"")))
     (assert-that
      (list-response-resources list-response)
      (has-length 0)))))

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

(defun only-id! (company)
  (oid (only! (scim-users-for-company company))))

(test 404-for-another-company-user
  (with-fixture state ()
    (let ((other-company (make-instance 'company)))
      (scim-post
       company
       example-post)
      (let ((id (only-id! company)))
        (finishes
         (scim-get company id))
        (signals does-not-exist
          (scim-get other-company id))))))


(test active-handling
  (with-fixture state ()
    (scim-post company example-post)
    (is-true (scim-user-activep (only! (scim-users-for-company company))))))

(test active-handling-false
  (with-fixture state ()
    (let ((external-user (make-instance 'external-user
                                        :external-id "foobar"
                                        :user-name "barbar@example.com"
                                        :emails (list
                                                 (make-instance 'external-email
                                                                :value "barbar@example.com"))
                                        :activep nil)))
      (is-false (external-user-activep external-user))
      (scim-post company
                 (encode-json
                  external-user)))
    (is-false (scim-user-activep (only! (scim-users-for-company company))))))

(test object-validation
  (with-fixture state ()
    (let ((external-user (make-instance 'external-user
                                        :external-id "foobar"
                                        :user-name "barbar@example.com"
                                        :emails
                                        (list (make-instance 'external-email
                                                             :type "primary"
                                                             :value "barbar@example.com")))))
      (finishes (validate-dto external-user)))))

(test object-validation-needs-user-name-as-email
  (with-fixture state ()
    (let ((external-user (make-instance 'external-user
                                        :external-id "foobar"
                                        :user-name "barbar"
                                        :emails
                                        (list (make-instance 'external-email
                                                             :type "primary"
                                                             :value "barbar@example.com")))))
      (signals user-name-must-be-email
        (validate-dto external-user)))))

(test object-validation-only-one-email-per-user
  (with-fixture state ()
    (let ((external-user (make-instance 'external-user
                                        :external-id "foobar"
                                        :user-name "barbar"
                                        :emails
                                        (list (make-instance 'external-email
                                                             :type "primary"
                                                             :value "barbar@example.com")
                                              (make-instance 'external-email
                                                             :type "primary"
                                                             :value "barbar2@example.com")))))
      (signals only-one-email
        (validate-dto external-user)))))

(test object-validation-needs-user-name-as-email
  (with-fixture state ()
    (let ((external-user (make-instance 'external-user
                                        :external-id "foobar"
                                        :user-name "barbar"
                                        :emails
                                        (list (make-instance 'external-email
                                                             :type "primary"
                                                             :value "barbar")))))
      (signals invalid-email
        (validate-dto external-user)))))


