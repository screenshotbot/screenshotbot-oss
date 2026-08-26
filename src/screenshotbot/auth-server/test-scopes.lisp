;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/auth-server/test-scopes
  (:use #:cl
        #:fiveam)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:screenshotbot/auth-server/scopes
                #:default-scopes
                #:find-scope
                #:parse-scope-string
                #:render-scope-list
                #:scopes-permissions
                #:supported-scope-names))
(in-package :screenshotbot/auth-server/test-scopes)

(util/fiveam:def-suite)

(test parse-scope-string-splits-on-spaces
  (assert-that (parse-scope-string "profile api:read")
               (contains "profile" "api:read")))

(test parse-scope-string-is-order-independent
  "The consent screen shouldn't reorder itself based on what the client typed."
  (is (equal (parse-scope-string "profile api:read")
             (parse-scope-string "api:read profile"))))

(test parse-scope-string-collapses-duplicates-and-extra-whitespace
  (assert-that (parse-scope-string "  profile   profile  ")
               (contains "profile")))

(test parse-scope-string-reports-unknown-scopes
  (multiple-value-bind (known unknown)
      (parse-scope-string "profile admin:everything")
    (assert-that known (contains "profile"))
    (assert-that unknown (contains "admin:everything"))))

(test parse-scope-string-handles-empty-input
  (multiple-value-bind (known unknown) (parse-scope-string nil)
    (is (equal nil known))
    (is (equal nil unknown)))
  (multiple-value-bind (known unknown) (parse-scope-string "")
    (is (equal nil known))
    (is (equal nil unknown))))

(test default-scopes-are-least-privileged
  "A client that asks for nothing must not get API access."
  (is (equal nil (scopes-permissions (default-scopes)))))

(test render-scope-list-round-trips
  (let ((scopes (supported-scope-names)))
    (assert-that (parse-scope-string (render-scope-list scopes))
                 (apply #'contains scopes))))

(test scopes-map-onto-api-key-permissions
  (is (equal nil (scopes-permissions '("profile"))))
  (is (equal '(:full) (scopes-permissions '("api:read"))))
  (is (equal '(:ci) (scopes-permissions '("api:write"))))
  (is (equal 2 (length (scopes-permissions '("api:read" "api:write"))))))

(test find-scope
  (is-true (find-scope "profile"))
  (is-false (find-scope "nope"))
  (is-false (find-scope "PROFILE")))
