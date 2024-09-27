;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/api-key-impl
  (:use #:cl)
  (:import-from #:screenshotbot/dashboard/api-keys
                #:permission
                #:api-key-available-permissions)
  (:import-from #:screenshotbot/installation
                #:installation))
(in-package :screenshotbot/dashboard/api-key-impl)

(named-readtables:in-readtable markup:syntax)

(defmethod api-key-available-permissions ((self installation))
  (list
   (make-instance
    'permission
    :name :ci
    :label
    <label class= "form-check-label" for= "#ci-access">
      CI Access<br/>
      <span class= "text-muted">Upload screenshots from CI jobs</span>
    </label>)
   (make-instance
    'permission
    :name :full
    :label
    <label class= "form-check-label" for= "#ci-access">
      Full access<br/>
      <span class= "text-muted">Suitable for custom scripts using the API</span>
    </label>)))

