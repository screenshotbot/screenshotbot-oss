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

(test the-generate-fn-exists
  (is (functionp #'screenshotbot/assets::generate-recorder-platform-assets)))

(define-platform-asset "dummy")

(test generate-.sh-uses-domain
  (with-installation (:installation (make-instance
                                     'installation
                                     :domain "https://example.com"))
   (with-test-store ()
     (generate-dummy-platform-assets)
     (assert-that (mapcar #'pathname-name
                          (fad:list-directory (path:catdir (object-store) "artifacts/")))
                  (has-item
                   "dummy"))
     (let ((contents (uiop:read-file-string (path:catfile (object-store) "artifacts/dummy.sh"))))
       (assert-that contents
                    (contains-string "https://example.com/artifact/dummy-linux"))))))

(test generate-.sh-uses-cdn
  (with-installation (:installation (make-instance
                                     'installation
                                     :cdn "https://cdn.example.com"
                                     :domain "https://example.com"))
   (with-test-store ()
     (generate-dummy-platform-assets)
     (let ((contents (uiop:read-file-string (path:catfile (object-store) "artifacts/dummy.sh"))))
       (assert-that contents
                    (contains-string "https://cdn.example.com/artifact/dummy-linux")
                    (is-not (contains-string "https://example.com/artifact/dummy-linux")))))))
