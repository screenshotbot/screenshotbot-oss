;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-assets
  (:use #:cl
        #:fiveam-matchers
        #:fiveam)
  (:import-from #:screenshotbot/assets
                #:generate-.sh
                #:define-platform-asset)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/store/store
                #:object-store
                #:with-test-store)
  (:import-from #:screenshotbot/installation
                #:installation))
(in-package :screenshotbot/test-assets)

(util/fiveam:def-suite)

(define-platform-asset "dummy")

(test generate-.sh-uses-domain
  (with-installation (:installation (make-instance
                                     'installation
                                     :domain "https://example.com"))
   (with-test-store ()
     (let ((contents (generate-.sh "dummy")))
       (assert-that contents
                    (contains-string "https://example.com/artifact/${VERSION}dummy-linux"))))))

(test generate-.sh-uses-cdn
  (with-installation (:installation (make-instance
                                     'installation
                                     :cdn "https://cdn.example.com"
                                     :domain "https://example.com"))
   (with-test-store ()
     (let ((contents (generate-.sh "dummy")))
       (assert-that contents
                    (contains-string "https://cdn.example.com/artifact/${VERSION}dummy-linux")
                    (is-not (contains-string "https://example.com/artifact/")))))))
