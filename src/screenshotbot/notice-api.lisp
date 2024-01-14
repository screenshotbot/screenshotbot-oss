;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(uiop/package:define-package :screenshotbot/notice-api
  (:use #:cl #:alexandria)
  (:import-from #:screenshotbot/model/invite
                #:invite-company)
  (:export
   #:invite-company
   #:notice-title
   #:notice-summary))
(in-package :screenshotbot/notice-api)
