;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-throttler
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/throttler
                #:*global-request-throttler*
                #:maybe-throttle-request)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:util/throttler
                #:ip-throttler))
(in-package :screenshotbot/test-throttler)


(util/fiveam:def-suite)

(defclass fake-request ()
  ((script-name :initarg :script-name
                :reader hunchentoot:script-name)))

(test by-default-function-returns-nil
  (is
   (eql nil
        (maybe-throttle-request :installation
                                (make-instance 'fake-request
                                               :script-name "/api/run"))))
  (is
   (eql nil
        (maybe-throttle-request :installation
                                (make-instance 'fake-request
                                               :script-name "/api/image"))))

  (is
   (eql nil
        (maybe-throttle-request :installation
                                (make-instance 'fake-request
                                               :script-name "/image/blob")))))

(test eventual-throttling
  (with-fake-request ()
    (let ((*global-request-throttler* (make-instance 'ip-throttler
                                                     :tokens 0)))
      (is
       (equal
        "Too Many Requests"
        (maybe-throttle-request
         :installation
         hunchentoot:*request*)))
      (is (eql 429 (hunchentoot:return-code hunchentoot:*reply*))))))
