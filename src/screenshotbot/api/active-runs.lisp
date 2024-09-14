;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/active-runs
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:defapi))
(in-package :screenshotbot/api/active-runs)

(defapi (%active-runs :uri "/api/runs/active" :method :get)
        (channel)
  "Find all the active runs for a given channel"
  (error "unimpl for ~a" channel))



