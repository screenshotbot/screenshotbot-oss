;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-github-oauth
  (:use :cl)
  (:import-from #:auth
                #:oauth-user-user
                #:user-email
                #:user-full-name)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:is
                #:is-false
                #:test
                #:with-fixture)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/login/github
                #:github-user)
  (:import-from #:screenshotbot/login/github-oauth
                #:prepare-gh-user)
  (:import-from #:screenshotbot/model/company
                #:prepare-singleton-company)
  (:import-from #:screenshotbot/user-api
                #:user
                #:user-image-url)
  (:import-from #:util/store/store
                #:with-test-store))
(in-package :screenshotbot/login/test-github-oauth)

(util/fiveam:def-suite)

(defvar *test-email* "arnold+test_github_oauth@example.com")

(defun %prepare-test-user ()
  (declare (optimize (debug 3)
                     (speed 0)))
  (let ((user (prepare-gh-user
               :emails (list *test-email*)
               :user-id 22
               :full-name "Arnold Noronha"
               :avatar "https://example.com/doofus.png")))
    user))

(def-fixture state ()
  (with-test-store ()
   (let ((*installation* (make-instance 'installation)))
     (prepare-singleton-company)
     (&body))))

(test slot-access-issue
  (with-fixture state ()
    (let ((obj (make-instance 'github-user :gh-user-id "foo")))
      (is (equal (oauth-user-user obj) nil)))))

(test create-new-user
  (with-fixture state ()
    (let ((user (%prepare-test-user)))
      (is (typep user 'user))
      (is-false (null (github-user user)))
      (is (equal *test-email* (user-email user)))
      (is (equal "Arnold Noronha" (user-full-name user)))
      (is (equal "https://example.com/doofus.png"
                 (user-image-url user))))))

(test second-time-with-the-same-user
  (with-fixture state ()
    (let ((user (%prepare-test-user)))
      (let ((user2 (%prepare-test-user)))
        (is (eql (store-object-id user) (store-object-id user2)))))))

(test second-time-change-args
  (with-fixture state ()
    (let ((user (%prepare-test-user)))
      (let* ((user (prepare-gh-user
                    :emails (list "sam@example.com")
                    :user-id 22
                    :full-name "Sam Currie"
                    :avatar "https://oddflowerstudio.com/profilepic")))
        (is (equal "sam@example.com" (user-email user)))
        (is (equal "Sam Currie" (user-full-name user)))
        (is (equal "https://oddflowerstudio.com/profilepic"
                   (user-image-url user)))))))
