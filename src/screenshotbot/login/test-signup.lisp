;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/test-signup
    (:use #:cl
          #:alexandria
          #:fiveam)
  (:Import-from #:../server
                #:*disable-mail*)
  (:import-from #:./signup
                #:signup-post)
  (:import-from #:screenshotbot/model/company
                #:*singleton-company*))

(util/fiveam:def-suite)

(defclass dummy-company ()
  ())

(test happy-path
  (util:with-fake-request (:host "localhost:80")
    (catch 'hunchentoot::handler-done
      (let ((*singleton-company*
              (make-instance 'dummy-company)))
       (auth:with-sessions ()
         (let ((*disable-mail* t))
           (signup-post  :email "arnold@tdrhq.com"
                         :password "foobar23"
                         :full-name "Arnold Noronha"
                         :accept-terms-p t
                         :plan :professional))
         (error "should not get here"))))
    (pass)))
