;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/slack/test-settings
  (:use :cl)
  (:import-from #:core/installation/installation
                #:*installation*)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:is
                #:test
                #:with-fixture)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:screenshotbot/model/company
                #:slack-config
                #:company
                #:default-slack-config)
  (:import-from #:screenshotbot/slack/core
                #:slack-tokens-for-company
                #:find-or-create-slack-config
                #:slack-token)
  (:import-from #:screenshotbot/slack/settings
                #:disconnect-slack
                #:post-settings-slack)
  (:import-from #:screenshotbot/user-api
                #:access-token)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :screenshotbot/slack/test-settings)

(util/fiveam:def-suite)

(defclass my-installation (multi-org-feature
                           installation)
  ())

(def-fixture state ()
  (let ((*installation* (make-instance 'my-installation)))
   (with-test-store ()
     (with-fake-request ()
       (let* ((company (make-instance 'company))
              (token (make-instance 'slack-token
                                     :access-token "foo"
                                     :company company
                                     :ts 34)))
         (setf (auth:request-account hunchentoot:*request*)
               company)
         (&body))))))

(test posting-when-nothing-is-available ()
  (with-fixture state ()
    (catch 'hunchentoot::handler-done
      (post-settings-slack
       "#general"
       t))
    (is (eql token (access-token (default-slack-config company))))))

(test find-or-create-should-create-if-not-exists
  (with-fixture state ()
    (is (typep (find-or-create-slack-config company)
               'slack-config))))

(test delete-slack-tokens
  (with-fixture state ()
    (make-instance 'slack-token :company company
                                :access-token "bar")
    (make-instance 'slack-token :company company
                                :access-token "car")
    (disconnect-slack company)
    (assert-that (slack-tokens-for-company company)
                 (has-length 0))))

(test delete-doesnt-delete-other-company-tokens
  (with-fixture state ()
    (let ((other-company (make-instance 'company)))
      (make-instance 'slack-token :company other-company
                                  :access-token "bar")
      (make-instance 'slack-token :company company
                                  :access-token "bar")
      (disconnect-slack company)
      (assert-that (slack-tokens-for-company company)
                   (has-length 0))
      (assert-that (slack-tokens-for-company other-company)
                   (has-length 1)))))
