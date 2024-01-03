;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :screenshotbot/user-api
    (:use #:cl #:alexandria)
  (:export
   #:current-user
   #:user
   #:signup-get
   #:user-full-name
   #:singletonp
   #:user-companies
   #:user-email
   #:user-image-url
   #:channel-name
   #:activep
   #:company-switch-page
   #:adminp
   #:can-view!
   #:can-view
   #:all-users
   #:channel-repo
   #:access-token
   #:commit-link
   #:company-runs
   #:recorder-run-commit
   #:activep
   #:recorder-previous-run
   #:pull-request-url
   #:company-runs
   #:%created-at
   #:current-company
   #:current-user
   #:user-api-keys
   #:commit-link
   #:recorder-run-screenshots
   #:recorder-run-channel
   #:company-channels
   #:channel-active-run
   #:company-name
   #:screenshot-name
   #:company-reports
   #:created-at
   #:report-num-changes
   #:model-id
   #:channel
   #:api-key-user
   #:api-key-company
   #:can-public-view
   #:unaccepted-invites
   #:user-notices
   #:personalp))
(in-package :screenshotbot/user-api)


(declaim (ftype (function ()) current-user))
(declaim (ftype (function (&key &allow-other-keys)) signup-get))
