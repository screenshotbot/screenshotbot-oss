;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/api/version
  (:use #:cl)
  (:import-from #:screenshotbot/server
                #:defhandler)
  (:import-from #:screenshotbot/api/model
                #:version
                #:*api-version*
                #:encode-json)
  (:import-from #:core/installation/installation
                #:installation-domain)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/api/core
                #:authenticate-api-request)
  (:import-from #:util/threading
                #:ignore-and-log-errors))
(in-package :screenshotbot/api/version)

(defhandler (api-version :uri "/api/version") ()
  (setf (hunchentoot:content-type*)
        "application/json; charset=utf-8")
  (when (hunchentoot:authorization)
    (ignore-and-log-errors ()
      ;; We don't need to be authenticated for this endpoint. In
      ;; particular the client may not be handling failure
      ;; properly. So we always return.
      (authenticate-api-request hunchentoot:*request*)))
  (log:debug "Got session id as: ~a" (hunchentoot:header-in* :x-cli-session-id))
  (encode-json
   (make-instance 'version
                  :version *api-version*
                  :url (installation-domain (installation))
                  :features
                  (%build-features (auth:current-company)))))

(defparameter *gk-list* `((:cli-shallow-clones t)
                          (:server-cli-logs nil)))

(defmethod %build-features ((company company))
  (loop for (gk default) in *gk-list*
        if (gk:check gk company :default default)
          collect (string-downcase (string gk))))

(defmethod %build-features ((company null))
  nil)
