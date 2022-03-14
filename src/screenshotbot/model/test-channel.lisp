;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/model/test-channel
  (:use #:cl
        #:alexandria
        #:./channel
        #:./recorder-run
        #:./company
        #:fiveam)
  (:import-from #:./channel
                #:get-full-repo-from-repo)
  (:import-from #:util/store
                #:with-test-store))

(util/fiveam:def-suite)

(defclass unserializable () ())

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test active-runs
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (run (make-instance 'recorder-run
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
     (let ((run (make-instance 'recorder-run :channel channel
                                             :company company)))
       (is (equal (list run)
                  (channel-runs channel)))))))

(test find-production-run-for-commit
  (with-fixture state ()
   (let* ((channel (make-instance 'channel))
          (foo-run (make-instance 'recorder-run :channel channel
                                                :commit-hash "foo"))
          (bar-run (make-instance 'recorder-run :channel channel
                                                :commit-hash "bar")))
     (is (eql foo-run (production-run-for channel :commit "foo")))
     (is (eql bar-run (production-run-for channel :commit "bar")))
     (let ((another-bar-run (make-instance 'recorder-run
                                            :channel channel
                                            :commit-hash "bar")))
       (is (eql bar-run (production-run-for channel :commit "bar")))))))

(test get-full-repo-from-repo
  (is (equal "foo/bar"
             (get-full-repo-from-repo "git@github.com:foo/bar.git"))))
