;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/replay/test-run-builder
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/replay/run-builder
                #:screenshots
                #:record-screenshot
                #:all-screenshots)
  (:import-from #:util/copy-file
                #:copy-file-fast)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/testing
                #:with-installation))
(in-package :screenshotbot/replay/test-run-builder)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-installation ()
     (let ((company (make-instance 'company)))
       (&body)))))

(test simple-record-screenshot
  (with-fixture state ()
   (let ((all-screenshots (make-instance 'all-screenshots
                                         :company company)))
     (record-screenshot
      all-screenshots
      :title "foobar"
      :md5 "aaaa"
      :fetch (lambda (tmpfile)
               (copy-file-fast (asdf:system-relative-pathname :screenshotbot "fixture/rose.png")
                               tmpfile)))
     (is (eql 1 (length (screenshots all-screenshots)))))))
