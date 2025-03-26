;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/login/test-cached-avatar
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:auth/login/cached-avatar
                #:write-avatar)
  (:import-from #:bknr.datastore
                #:class-instances
                #:blob-pathname)
  (:import-from #:auth/avatar
                #:content-type
                #:overriden-avatar)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:assert-that))
(in-package :auth/login/test-cached-avatar)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test write-new-avatar
  (with-fixture state ()
    (finishes
      (write-avatar 'user1
                    :res #(65 66 67)
                    :headers `((:content-type . "image/jpg"))))
    (let ((oa (first (class-instances 'overriden-avatar))))
      (is
       (equal
        "ABC"
        (uiop:read-file-string
         (blob-pathname oa))))
      (is
       (equal
        "image/jpg"
        (content-type oa))))
    (finishes
      (write-avatar 'user1
                    :res #(66 67 68)
                    :headers `((:content-type . "image/png"))))
    (assert-that (class-instances 'overriden-avatar)
                 (has-length 1))
    (let ((oa (first (class-instances 'overriden-avatar))))
      (is
       (equal
        "BCD"
        (uiop:read-file-string
         (blob-pathname oa))))
      (is
       (equal
        "image/png"
        (content-type oa))))))

