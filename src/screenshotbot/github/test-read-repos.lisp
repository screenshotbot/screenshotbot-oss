;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/test-read-repos
  (:use #:cl
        #:fiveam)
  (:import-from #:util/mock-recording
                #:track
                #:with-recording)
  (:import-from #:screenshotbot/github/read-repos
                #:can-edit-repo
                #:whoami)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:screenshotbot/github/access-checks
                #:github-api-request)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/github/test-read-repos)

(util/fiveam:def-suite)

#|
To record this test, you need an access token with GitHub.

You can use a personal access
token (https://github.com/settings/tokens), run the tests, and then
delete the token. Leave the token here.

The token should have permission to access all repositories, and
should have read-only access to `Metadata`.

The repos used here are the ones that I have in my account. So if
you're not me, you might have to modify the repo names if you're
re-recording this test.

|#

(defvar *access-token*
  "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")

(def-fixture state (name &key (record nil))
  (with-test-store ()
   (let ((pathname (asdf:system-relative-pathname
                    :screenshotbot
                    (format nil "github/fixture/~a.rec" name))))
     (with-recording (pathname :record record)
       (track 'github-api-request)
       (&body)))))

(defmacro %test (name (&rest args) &body body)
  `(test ,name
     (with-fixture state (,(str:downcase name) ,@args)
       ,@body)))

(%test whoami-integration-test ()
  (is (equal "tdrhq"
             (whoami *access-token*))))

(%test can-edit-simple-repo ()
  (is-true (can-edit-repo *access-token*  "https://github.com/tdrhq/slite")))

(%test can-edit-for-private-repo ()
  (is-true (can-edit-repo *access-token* "https://github.com/tdrhq/web")))

(%test cannot-edit-repo-owned-somewhere-else ()
  (assert-that
   (multiple-value-list (can-edit-repo *access-token*
                                       "https://github.com/pointfreeco/swift-snapshot-testing"))
   (contains
    nil
    (contains-string "not accessible"))))
