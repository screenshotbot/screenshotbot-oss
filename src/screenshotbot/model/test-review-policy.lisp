;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-review-policy
  (:use #:cl
        #:fiveam)
  (:import-from #:cl-mock
                #:with-mocks)
  (:import-from #:screenshotbot/model/channel
                #:review-policy)
  (:import-from #:screenshotbot/user-api
                #:channel)
  (:import-from #:screenshotbot/model/user
                #:make-user)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/review-policy
                #:parse-email
                #:can-review?
                #:disallow-author-review-policy)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run))
(in-package :screenshotbot/model/test-review-policy)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
   (with-mocks ()
     (with-test-store ()
       (let ((disallow-author (make-instance 'disallow-author-review-policy))
             (user (make-user :email "foo@example.com")))
         (&body))))))

(test simple-can-review
  (with-fixture state ()
    (is-true
     (can-review? disallow-author
                  (make-recorder-run :author nil)
                  user))
    (is-true
     (can-review? disallow-author
                  (make-recorder-run :author "bar@example.com")
                  user))
    (is-false
     (can-review? disallow-author
                  (make-recorder-run :author "foo@example.com")
                  user))))

(test parses-out-email
  (with-fixture state ()
    (is-false
     (can-review? disallow-author
                  (make-recorder-run :author "Foo Bar <foo@example.com>")
                  user))))

(test parse-email
  (with-fixture state ()
    (is (equal "foo@example.com" (parse-email "Foo bar <foo@example.com>")))))
