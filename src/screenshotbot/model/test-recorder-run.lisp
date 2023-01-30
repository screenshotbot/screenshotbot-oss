;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-recorder-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/recorder-run
                #:pull-request-id
                #:convert-old-promotion-logs
                #:transient-promotion-log
                #:promotion-log
                #:recorder-run)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/core
                #:equal-to
                #:has-typep
                #:assert-that)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-recorder-run)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((run (make-instance 'recorder-run))
          (promotion-log (make-instance 'promotion-log)))
     (&body))))


(test promotion-log-object-for-old-object
  (with-fixture state ()
    (with-transaction ()
     (setf (slot-value run 'promotion-log) promotion-log))
    (assert-that (promotion-log run)
                (equal-to promotion-log))))

(test promotion-log-for-new
  (with-fixture state ()
    (assert-that (promotion-log run)
                 (has-typep 'transient-promotion-log))
    (is (pathnamep
         (blob-pathname (promotion-log run))))))

(test convert-old-promotion-logs
  (with-fixture state ()
    (with-transaction ()
      (setf (slot-value run 'promotion-log) promotion-log))
    (with-open-file (out (blob-pathname (promotion-log run)) :direction :output)
      (write-string "zoidberg" out))

    (convert-old-promotion-logs)
    (is (equal "zoidberg" (uiop:read-file-string (blob-pathname (promotion-log run)))))))

(test pull-request-id
  (with-fixture state ()
    (let ((run2 (make-instance 'recorder-run
                               :pull-request "https://foo/bar/20")))
      (is (eql nil (pull-request-id run)))
      (is (eql 20 (pull-request-id run2))))))
