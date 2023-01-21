;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-screenshot-key
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/screenshot
                #:make-screenshot
                #:make-key-from-screenshot
                #:make-screenshot-from-key)
  (:import-from #:screenshotbot/model/screenshot-key
                #:ensure-screenshot-key
                #:screenshot-key)
  (:import-from #:screenshotbot/user-api
                #:screenshot-name)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:screenshotbot/screenshot-api
                #:screenshot-image))
(in-package :screenshotbot/model/test-screenshot-key)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test simple-conversion
  (with-fixture state ()
    (let ((screenshot (make-screenshot :name "foobar2")))
      (is (eql
           (make-key-from-screenshot screenshot)
           (make-key-from-screenshot screenshot)))
      (is (equal "foobar2"
                 (screenshot-name
                  (make-key-from-screenshot screenshot))))
      (assert-that (make-key-from-screenshot screenshot)
                   (has-typep 'screenshot-key)))))

(test reverse-conversion
  (with-fixture state ()
    (let ((key (ensure-screenshot-key :name "bleh")))
      (is (eql key (ensure-screenshot-key :name "bleh")))
      (is (not (eql key (ensure-screenshot-key :name "bleh"
                                               :lang "blah"))))
      (let ((screenshot (make-screenshot-from-key key :image)))
        (is (equal "bleh" (screenshot-name screenshot)))
        (is (eql :image (screenshot-image screenshot)))))))
