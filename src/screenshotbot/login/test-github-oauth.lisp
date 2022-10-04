;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/test-github-oauth
  (:use #:cl
        #:alexandria
        #:bknr.datastore
        #:fiveam
        #:screenshotbot/model/user
        #:screenshotbot/model/github
        #:screenshotbot/api-key-api)
  (:import-from #:./github-oauth
                #:prepare-gh-user)
  (:import-from #:bknr.datastore
                #:store-object-id)
  (:import-from #:screenshotbot/installation
                #:*installation*
                #:installation)
  (:import-from #:screenshotbot/model/company
                #:prepare-singleton-company)
  (:import-from #:util/store
                #:with-test-store))

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

(defmacro dlet (((k v)) &body body)
  `(progn
     (let ((,k ,v))
       (unwind-protect
            (progn ,@body)
         (let ((gh (github-user ,k)))
           (delete-object gh)
           (delete-object ,k))))))

(test create-new-user
  (with-fixture state ()
    (dlet ((user (%prepare-test-user)))
      (is (typep user 'user))
      (is-false (null (github-user user)))
      (is (equal *test-email* (user-email user)))
      (is (equal "Arnold Noronha" (user-full-name user)))
      (is (equal "https://example.com/doofus.png"
                 (user-image-url user))))))

(test second-time-with-the-same-user
  (with-fixture state ()
    (dlet ((user (%prepare-test-user)))
     (let ((user2 (%prepare-test-user)))
       (is (eql (store-object-id user) (store-object-id user2)))))))

(test second-time-change-args
  (with-fixture state ()
    (dlet ((user (%prepare-test-user)))
     (let* ((user (prepare-gh-user
                   :emails (list "sam@example.com")
                   :user-id 22
                   :full-name "Sam Currie"
                   :avatar "https://oddflowerstudio.com/profilepic")))
       (is (equal "sam@example.com" (user-email user)))
       (is (equal "Sam Currie" (user-full-name user)))
       (is (equal "https://oddflowerstudio.com/profilepic"
                  (user-image-url user)))))))
