;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/login/test-verify-email
  (:use #:cl
        #:fiveam)
  (:import-from #:auth/login/verify-email
                #:*throttler*
                #:enter-code-screen/post)
  (:import-from #:core/installation/installation
                #:abstract-installation
                #:*installation*)
  (:import-from #:screenshotbot/login/common
                #:auth-template-impl)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:assert-that)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:util/throttler
                #:ip-throttler)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :auth/login/test-verify-email)


(util/fiveam:def-suite)

(defclass my-installation (abstract-installation)
  ())

(defmethod auth-template-impl ((self my-installation) body &key &allow-other-keys)
  body)

(def-fixture state ()
  (with-fake-request ()
    (auth:with-sessions ()
     (let ((*throttler* (make-instance 'ip-throttler :tokens 300)))
       (let ((*installation* (make-instance 'my-installation)))
         (let ((state (make-instance 'auth/login/verify-email::state
                                     :code 123456
                                     :email "foo@example.com"
                                     :redirect "/runs")))
           (&body)))))))

(defun %post-errors (state code)
  (nth-value
   1
   (enter-code-screen/post state :entered-code code)))

(test simple-post-error
  (with-fixture state ()
    (assert-that (%post-errors state "22")
                 (has-length 1)
                 (contains
                  (is-equal-to
                   '(:ENTERED-CODE . "The code should be a six digit number that we sent to foo@example.com"))))))

(test too-many-attempts
  (with-fixture state ()
    (loop for i from 0 below 20
          do 
          (%post-errors state "22"))
    (assert-that (%post-errors state "123456")
                 (has-length 1)
                 (contains
                  (is-equal-to
                   '(:ENTERED-CODE . "Too many attempts, please try signing up again"))))))
