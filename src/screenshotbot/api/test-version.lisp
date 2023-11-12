;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :screenshotbot/api/test-version
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/api/version
                #:api-version)
  (:import-from #:screenshotbot/api/model
                #:installation-url
                #:version-number
                #:*api-version*
                #:version
                #:decode-json)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :screenshotbot/api/test-version)

(util/fiveam:def-suite)

(test api-version-happy-path
  (with-installation ()
    (with-fake-request ()
     (let ((content (api-version)))
       (let ((version (decode-json content 'version)))
         (is (eql *api-version* (version-number version)))
         (is (equal "https://example.com" (installation-url version))))))))
