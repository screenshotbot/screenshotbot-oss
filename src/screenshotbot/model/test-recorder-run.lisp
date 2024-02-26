;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-recorder-run
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run-author
                #:%author
                #:assert-no-loops
                #:runs-for-tag
                #:channel-runs
                #:make-recorder-run
                #:pull-request-id
                #:transient-promotion-log
                #:promotion-log
                #:recorder-run)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:equal-to
                #:has-typep
                #:assert-that)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:bknr.datastore
                #:blob-pathname)
  (:import-from #:fiveam-matchers/lists
                #:has-item
                #:contains)
  (:import-from #:screenshotbot/user-api
                #:recorder-previous-run
                #:recorder-run-commit
                #:channel)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:screenshotbot/model/channel
                #:production-run-for)
  (:import-from #:screenshotbot/model/company
                #:company)

  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/test-recorder-run)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((run (make-recorder-run))
          (company (make-instance 'company))
          (promotion-log (make-instance 'promotion-log)))
     (&body))))


(test promotion-log-for-new
  (with-fixture state ()
    (assert-that (promotion-log run)
                 (has-typep 'transient-promotion-log))
    (is (pathnamep
         (blob-pathname (promotion-log run))))))

(test pull-request-id
  (with-fixture state ()
    (let ((run2 (make-recorder-run
                 :pull-request "https://foo/bar/20")))
      (is (eql nil (pull-request-id run)))
      (is (eql 20 (pull-request-id run2))))))

(test maintains-channel-runs
  (with-fixture state ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run
                 :commit-hash "bleh2"
                 :channel channel)))
      (assert-that (production-run-for channel :commit "car")
                   (has-length 0))
      (assert-that (channel-runs channel)
                   (contains run))
      (assert-that (production-run-for channel :commit "bleh2")
                   (is-equal-to run))
      (bknr.datastore:delete-object run)
      (assert-that (channel-runs channel)
                   (has-length 0))
      (assert-that (production-run-for channel :commit "bleh2")
                   (has-length 0)))))

(test if-commit-is-not-present-try-override-commit
  (with-fixture state ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run
                 :override-commit-hash "bleh")))
      (is (equal "bleh" (recorder-run-commit run))))))

(test if-both-commit-and-override-commit-is-not-present
  "Then we return nil without failing"
  (with-fixture state ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run)))
      (is (equal nil (recorder-run-commit run))))))

(test finding-runs-by-tags
  (with-fixture state ()
    (let* ((company2 (make-instance 'company))
           (run1 (make-recorder-run
                     :company company
                     :tags (list "foo" "bar")))
           (run2 (make-recorder-run
                  :company company
                  :tags (list "bar")))
           (run3 (make-recorder-run
                  :company company))
           (run4 (make-recorder-run
                  :company company2
                  :tags (list "foo"))))
      (assert-that (runs-for-tag company "foo")
                   (contains run1))
      (assert-that (runs-for-tag company "bar")
                   (has-item run1)
                   (has-item run2)))))

(test assert-no-loops
  (with-fixture state ()
    (let ((run (make-recorder-run
                :previous-run (make-recorder-run
                               :previous-run (make-recorder-run
                                              :previous-run nil)))))
      (finishes (assert-no-loops run))
      (setf (recorder-previous-run (recorder-previous-run (recorder-previous-run run)))
            run)
      (signals simple-error
        (assert-no-loops run)))
    (let ((run (make-recorder-run
                :previous-run (make-recorder-run
                               :previous-run nil))))
      (finishes (assert-no-loops run)))))

(test unbound-slot-for-author
  (with-fixture state ()
    (let ((run (make-recorder-run)))
      (slot-makunbound run '%author)
      (is (eql nil (recorder-run-author run))))))
