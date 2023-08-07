;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/test-channel
  (:use #:cl
        #:alexandria
        #:screenshotbot/model/channel
        #:screenshotbot/model/recorder-run
        #:screenshotbot/model/company
        #:fiveam)
  (:import-from #:screenshotbot/model/channel
                #:get-full-repo-from-repo)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:fiveam-matchers/strings
                #:contains-string)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:screenshotbot/model/recorder-run
                #:unchanged-run
                #:make-recorder-run))

(util/fiveam:def-suite)

(defclass unserializable () ())

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test active-runs
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (run (make-recorder-run
                :channel channel)))
     (is (equal nil (active-run channel "foo")))
     (setf (active-run channel "foo") run)
     (is (eql run (active-run channel "foo"))))))

(test production-run-for
  (with-fixture state ()
   (let* ((company (make-instance 'company))
          (channel (make-instance 'channel
                                   :company company)))
     (is (equal nil (channel-runs channel)))
     (let ((run (make-recorder-run :channel channel
                                   :company company)))
       (is (equal (list run)
                  (channel-runs channel)))))))

(test find-production-run-for-commit
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (foo-run (make-recorder-run :channel channel
                                      :commit-hash "foo"))
          (bar-run (make-recorder-run :channel channel
                                      :commit-hash "bar")))
     (is (eql foo-run (production-run-for channel :commit "foo")))
     (is (eql bar-run (production-run-for channel :commit "bar")))
     (let ((another-bar-run (make-recorder-run
                             :channel channel
                             :commit-hash "bar")))
       (is (eql bar-run (production-run-for channel :commit "bar")))))))

(test find-production-run-unchanged-run
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (unchanged-run (make-instance 'unchanged-run
                                  :channel channel
                                  :commit "foo"
                                  :other-commit "bar"))
          (bar-run (make-recorder-run :channel channel
                                      :commit-hash "bar")))
     (is (eql bar-run (production-run-for channel :commit "bar")))
     (is (eql bar-run (production-run-for channel :commit "foo")))

     (let ((another-bar-run (make-recorder-run
                             :channel channel
                             :commit-hash "bar")))
       (is (eql bar-run (production-run-for channel :commit "foo")))
       (is (eql bar-run (production-run-for channel :commit "bar")))))))

(test detect-infinite-loops-in-unchanged-run
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (foo-run (make-instance 'unchanged-run
                                  :channel channel
                                  :commit "foo"
                                  :other-commit "bar"))
          (bar-run (make-instance 'unchanged-run
                                  :channel channel
                                  :commit "bar"
                                  :other-commit "foo")))
     (is (eql nil (production-run-for channel :commit "bar"))))))

(test get-full-repo-from-repo
  (is (equal "foo/bar"
             (get-full-repo-from-repo "git@github.com:foo/bar.git"))))


(test channel-print-object
  (with-fixture state ()
    (is (equal "#<CHANNEL foobar>"
               (prin1-to-string
                (make-instance 'channel
                               :name "foobar"))))
    (assert-that (prin1-to-string (make-instance 'channel))
                 (contains-string "CHANNEL"))))
