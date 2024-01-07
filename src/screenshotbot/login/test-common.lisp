;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-common
  (:use :cl)
  (:import-from #:fiveam-matchers/core
                #:assert-that
                #:has-typep
                #:is-equal-to)
  (:import-from #:it.bese.fiveam
                #:def-fixture
                #:test
                #:with-fixture)
  (:import-from #:screenshotbot/login/common
                #:signin-get)
  (:import-from #:screenshotbot/server
                #:server-with-login)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/login/test-common)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (cl-mock:with-mocks ()
       (&body)))))

(test server-with-login-happy-path
  (with-fixture state ()
    (let ((last-redirect))
      (cl-mock:if-called 'signin-get
                         (lambda (&key alert redirect)
                           (setf last-redirect redirect)))
      (server-with-login
       (lambda () "foo")
       :needs-login t)
      (assert-that last-redirect
                   (has-typep 'nibble:nibble)))))

(test server-with-login-with-allowing-url-redirect
  (with-fixture state ()
    (let ((last-redirect))
      (cl-mock:if-called 'signin-get
                         (lambda (&key alert redirect)
                           (setf last-redirect redirect)))
      (with-fake-request (:script-name "/foo/bar")
        (server-with-login
         (lambda () "foo")
         :allow-url-redirect t
         :needs-login t))
      (assert-that last-redirect
                   (is-equal-to "/foo/bar")))))
