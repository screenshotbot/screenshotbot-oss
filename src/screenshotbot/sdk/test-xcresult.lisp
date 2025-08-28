;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/sdk/test-xcresult
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/sdk/xcresult
                #:parse-name
                #:xcresults-attachment-bundle)
  (:import-from #:screenshotbot/sdk/bundle
                #:list-images)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length))
(in-package :screenshotbot/sdk/test-xcresult)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((bundle (make-instance 'xcresults-attachment-bundle :directory
                               (asdf:system-relative-pathname :screenshotbot.sdk "fixture/xcresults-attachments/"))))
    (&body)))

(test simple-parsing
  (with-fixture state ()
    (assert-that (list-images bundle)
                 (has-length 6))))


(test parse-suggested-name
  (is
   (equal "SimpleProjectTests/testLoginViewSnapshot.1_0"
    (parse-name
     ;; see manifest.json
     "testLoginViewSnapshot.1_0_FA32D8D3-B6A9-4B7E-A958-921F53BBC2FE.png"
     "SimpleProjectTests/testLoginViewSnapshot()"))))
