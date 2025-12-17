;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/run-warnings
  (:use #:cl)
  (:import-from #:screenshotbot/dashboard/run-page
                #:warning-alert
                #:render-run-warning)
  (:import-from #:screenshotbot/model/core
                #:non-root-object)
  (:import-from #:bknr.datastore
                #:persistent-class)
  (:import-from #:screenshotbot/model/recorder-run
                #:base-run-warning)
  (:export
   #:azure-unauthorized-warning))
(in-package :screenshotbot/azure/run-warnings)

(named-readtables:in-readtable markup:syntax)

(defclass azure-unauthorized-warning (base-run-warning)
  ()
  (:metaclass persistent-class))


(defmethod render-run-warning (run (self azure-unauthorized-warning))
  <warning-alert type= "danger" call-out= "" >
    <span>Failed to update Azure build status. Please verify that the Personal Access Token is still valid, and has permissions to access this project. You can update the token <a href= "/settings/azure">here</a>.</span>
  </warning-alert>)


