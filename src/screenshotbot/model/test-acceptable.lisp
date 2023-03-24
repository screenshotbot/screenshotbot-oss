;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-acceptable
  (:use #:cl
        #:alexandria
        #:fiveam
        #:screenshotbot/model/report)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run
                #:recorder-run)
  (:import-from #:screenshotbot/model/channel
                #:channel)
  (:import-from #:screenshotbot/model/report
                #:find-acceptables))
(in-package :screenshotbot/model/test-acceptable)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let* ((channel (make-instance 'channel))
           (run (make-recorder-run
                 :channel channel
                 :pull-request "https://github.com/tdrhq/fast-example/pulls/20"))
           (report (make-instance 'report :run run
                                  :channel channel))
           (channel2 (make-instance 'channel))
           (run2 (make-recorder-run
                  :channel channel2
                  :pull-request "https://github.com/tdrhq/blah/pulls/20"))
           (report2 (make-instance 'report :run run
                                           :channel channel2))
           (run3 (make-recorder-run
                  :channel channel
                  :pull-request "https://github.com/tdrhq/fast-example/pulls/21"))
           (report3 (make-instance 'report :run run3
                                   :channel channel)))
     (&body))))

(test simple-acceptable-set-get
  (with-fixture state ()
   (let ((a (make-instance 'base-acceptable)))
     (is (equal nil (acceptable-state a)))
     (setf (acceptable-state a) :accepted)
     (is (equal :accepted (acceptable-state a)))
     (signals error
       (setf (acceptable-state a) :foo)))))

(test find-acceptables
  (with-fixture state ()
    (let ((acc (make-instance 'base-acceptable
                            :report report)))
      (is (equal
           (list acc)
           (find-acceptables channel
                             :pull-request-id 20)))
      (make-instance 'base-acceptable
                     :report report2)
      (is (equal
           (list acc)
           (find-acceptables channel
                             :pull-request-id 20)))
      (make-instance 'base-acceptable
                     :report report3)

      (is (equal
           (list acc)
           (find-acceptables channel
                             :pull-request-id 20))))))
