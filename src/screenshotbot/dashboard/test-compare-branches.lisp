;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/dashboard/test-compare-branches
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/testing
                #:with-test-user
                #:with-installation
                #:screenshot-test)
  (:import-from #:screenshotbot/dashboard/compare-branches
                #:resolve-commits
                #:%post
                #:%perform
                #:%form)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:cl-mock
                #:answer)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/recorder-run
                #:make-recorder-run))
(in-package :screenshotbot/dashboard/test-compare-branches)


(util/fiveam:def-suite)


(def-fixture state ()
  (with-test-store ()
   (cl-mock:with-mocks ()
     (with-installation ()
       (with-test-user (:company company
                        :user user
                        :logged-in-p t)
         (auth:with-sessions ()
           (&body)))))))

(screenshot-test compare-branches-form
  (with-fixture state ()
    (%form)))


(test %post-happy-path-without-result
  (with-fixture state ()
    (answer (%perform :sha1 "ab" :sha2 "cd" :repo "foo")
      "hello")
    (answer (resolve-commits company "ab" :repo "foo") (list :run1))
    (answer (resolve-commits company "cd" :repo "foo") (list :run2))    
    (is
     (equal "hello"
            (%post :sha1 "ab" :sha2 "cd" :repo "foo")))))
