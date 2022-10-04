;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(pkg:define-package :screenshotbot/login/test-signup
    (:use #:cl
          #:alexandria
          #:fiveam)
  (:Import-from #:screenshotbot/server
                #:*disable-mail*)
  (:import-from #:screenshotbot/login/signup
                #:signup-post)
  (:import-from #:screenshotbot/model/company
                #:prepare-singleton-company
                #:get-singleton-company
                #:company)
  (:import-from #:screenshotbot/installation
                #:*installation*
                #:installation)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:util/testing
                #:with-fake-request))

(util/fiveam:def-suite)

;; NOTE TO FUTURE SELF: This is a badly written test. This note was
;; added later. This test doesn't run very well in the repl,
;; AFAICT. Works fine on `make test-lw`
(test happy-path
  (with-test-store ()
   (with-fake-request (:host "localhost:80")
     (let ((*installation* (make-instance 'installation)))
       (prepare-singleton-company)
       (catch 'hunchentoot::handler-done
         (let* ((company (get-singleton-company *installation*)))
           (unwind-protect
                (auth:with-sessions ()
                  (let ((*disable-mail* t))
                    (let ((ret (signup-post  :email "arnold@tdrhq.com"
                                             :password "foobar23"
                                             :full-name "Arnold Noronha"
                                             :accept-terms-p t
                                             :plan :professional)))
                      (error "should not get here: ~s" ret))))
             (bknr.datastore:delete-object company)))))
     (pass))))
