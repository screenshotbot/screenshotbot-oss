;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-downloadable-run
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/downloadable-run
                #:build-archive
                #:delete-old-runs
                #:downloadable-run
                #:find-or-create-downloadable-run)
  (:import-from #:bknr.datastore
                #:blob-pathname
                #:class-instances)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:screenshotbot/model/image
                #:make-image)
  (:import-from #:screenshotbot/screenshot-api
                #:make-screenshot)
  (:import-from #:screenshotbot/testing
                #:with-installation))
(in-package :screenshotbot/model/test-downloadable-run)

(util/fiveam:def-suite)

(defvar *image* (asdf:system-relative-pathname :screenshotbot "fixture/rose.png"))

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (let* ((company (make-instance 'company))
            (img (make-image :pathname *image*))
            (run (make-recorder-run
                  :screenshots (list
                                (make-screenshot
                                 :image img
                                 :name "foobar")))))
       (&body)))))

(test find-or-create
  (with-fixture state ()
    (is (eql
         (find-or-create-downloadable-run run)
         (find-or-create-downloadable-run run)))
    (is-true (find-or-create-downloadable-run run))))

(test delete-old-runs
  (with-fixture state ()
    (let* ((d1 (find-or-create-downloadable-run run))
           (d2 (make-instance 'downloadable-run
                              :created-at 10
                              :run (make-recorder-run)))
           (pathname (bknr.datastore:blob-pathname d2)))
      (with-open-file (file pathname :direction :output))
      (assert-that (class-instances 'downloadable-run)
                   (has-length 2))
      (is (path:-e pathname))
      (delete-old-runs)
      (assert-that (class-instances 'downloadable-run)
                   (contains d1))
      (is (not (path:-e pathname))))))


(test delete-old-runs-when-the-file-does-not-exist
  (with-fixture state ()
    (let* ((d1 (find-or-create-downloadable-run run))
           (d2 (make-instance 'downloadable-run
                              :created-at 10
                              :run (make-recorder-run)))
           (pathname (bknr.datastore:blob-pathname d2)))
      (is (not (path:-e pathname)))
      (delete-old-runs)
      (assert-that (class-instances 'downloadable-run)
                   (contains d1)))))

(test build-archive
  (with-fixture state ()
    (let ((d1 (find-or-create-downloadable-run run)))
      (finishes
        (build-archive d1))
      (zip:with-zipfile (reader (blob-pathname d1))
        (assert-that (alexandria:hash-table-keys (zip:zipfile-entries reader))
                     (contains "foobar.png"))))))
