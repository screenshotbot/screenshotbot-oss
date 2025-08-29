;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-build-info
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/build-info
                #:build-url
                #:find-or-create-build-info)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:with-test-user))
(in-package :screenshotbot/model/test-build-info)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:company company)
     (&body))))

(test find-or-create-build-info-test
  (with-fixture state ()
   (let ((build-url "https://example.com/build/123"))
     ;; Test creating new build-info
     (let ((build-info-1 (find-or-create-build-info company build-url)))
       (is (not (null build-info-1)))
       (is (string= build-url (build-url build-info-1)))
      
       ;; Test finding existing build-info
       (let ((build-info-2 (find-or-create-build-info company build-url)))
         (is (eq build-info-1 build-info-2)))))))


