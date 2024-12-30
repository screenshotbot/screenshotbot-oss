;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/login/test-sso
  (:use #:cl
        #:fiveam)
  (:import-from #:auth/login/sso
                #:maybe-redirect-for-sso
                #:needs-sso-condition
                #:with-handle-needs-sso)
  (:import-from #:util/testing
                #:with-fake-request))
(in-package :auth/login/test-sso)

(util/fiveam:def-suite)

(test happy-path
  (is (equal "foobar"
             (with-handle-needs-sso ()
               "foobar"))))

(test simple-no-access-error-with-sso-issue
  (signals auth:no-access-error
    (with-handle-needs-sso ()
      (error 'auth:no-access-error))))

(defmethod maybe-redirect-for-sso ((company (eql 'foobar)) final-redirect)
  (hex:safe-redirect "/"))

(test sso-signal
  (with-fake-request ()
    (auth:with-sessions ()
     (is
      (equal nil ;; from the redirect
             (catch 'hunchentoot::handler-done
               (with-handle-needs-sso ()
                 (signal 'needs-sso-condition :company 'foobar)
                 (error 'auth:no-access-error))))))))

