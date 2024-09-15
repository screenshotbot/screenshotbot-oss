;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/active-runs
  (:use #:cl)
  (:import-from #:screenshotbot/api/core
                #:defapi)
  (:import-from #:screenshotbot/api/recorder-run
                #:run-to-dto)
  (:import-from #:screenshotbot/model/company
                #:find-channel)
  (:import-from #:screenshotbot/model/channel
                #:all-active-runs)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
(in-package :screenshotbot/api/active-runs)

(defapi (%active-runs :uri "/api/runs/active" :method :get
                      :wrap-success nil)
        (channel)
  "Find all the active runs for a given channel"
  (let ((channel (find-channel (auth:current-company) channel)))
    (or
     (when channel
       (loop for (nil . run) in (all-active-runs channel)
             if (auth:can-viewer-view (auth:viewer-context hunchentoot:*request*)
                                      run)
               collect (run-to-dto run)))
     (make-array 0))))



