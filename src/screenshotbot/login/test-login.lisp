;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-login
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/login/common
                #:signin-get)
  (:import-from #:util/testing
                #:screenshot-static-page
                #:with-fake-request)
  (:import-from #:util/form-errors
                #:with-form-errors)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/login/test-login)


(util/fiveam:def-suite)

(test login-screenshot-test
  (with-installation ()
   (with-fake-request ()
     (auth:with-sessions ()
       (screenshot-static-page
        :screenshotbot
        "login"
        (markup:write-html
         (signin-get)))))))

(test login-error-screen
  (with-installation ()
   (with-fake-request ()
     (auth:with-sessions ()
       (screenshot-static-page
        :screenshotbot
        "login-error-screen"
        (markup:write-html
         (with-form-errors (:errors `((:password . "Incorrect password"))
                            :password "foo"
                            :email "blah@gmail.com"
                            :was-validated t)
           (signin-get))))))))
