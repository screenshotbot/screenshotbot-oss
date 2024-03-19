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
                #:find-or-create-slack-config
                #:slack-token)
  (:import-from #:screenshotbot/slack/settings
                #:post-settings-slack)
  (:import-from #:screenshotbot/user-api
                #:access-token)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request))
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
