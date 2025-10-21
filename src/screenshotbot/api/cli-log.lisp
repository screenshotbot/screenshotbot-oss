;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/cli-log
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:defapi))
(in-package :screenshotbot/api/cli-log)

(defapi (nil :uri "/api/cli-log" :method :post) ()
  (log:info "CLI logs (session: ~a): ~a"
            (hunchentoot:header-in* :x-cli-session-id)
            (hunchentoot:raw-post-data :force-text t))
  nil)

