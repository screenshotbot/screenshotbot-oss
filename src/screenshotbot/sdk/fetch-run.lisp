;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/fetch-run
  (:use #:cl)
  (:import-from #:screenshotbot/sdk/api-context
                #:api-context)
  (:import-from #:screenshotbot/sdk/sdk
                #:request))
(in-package :screenshotbot/sdk/fetch-run)

(defun fetch-run (api-context oid)
  (let ((body (request api-context
                       (format nil "/api/run/~a" oid)
                       :method :get)))
    body))


