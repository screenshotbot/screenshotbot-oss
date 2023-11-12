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
                #:installation))
(in-package :screenshotbot/api/version)

(defhandler (api-version :uri "/api/version") ()
  (encode-json
   (make-instance 'version
                  :version *api-version*
                  :url (installation-domain (installation)))))
