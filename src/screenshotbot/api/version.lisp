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
                #:authenticate-api-request))
(in-package :screenshotbot/api/version)

(defhandler (api-version :uri "/api/version") ()
  (setf (hunchentoot:content-type*)
        "application/json; charset=utf-8")
  (when (hunchentoot:authorization)
    (authenticate-api-request hunchentoot:*request*))
  (encode-json
   (make-instance 'version
                  :version *api-version*
                  :url (installation-domain (installation))
                  :features
                  (%build-features (auth:current-company)))))

(defparameter *gk-list* (list :cli-shallow-clones
                              :server-cli-logs))

(defmethod %build-features ((company company))
  (loop for gk in *gk-list*
        if (gk:check gk company)
          collect (string-downcase (string gk))))

(defmethod %build-features ((company null))
  nil)
