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
                #:with-test-user))
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
