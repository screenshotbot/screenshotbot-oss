;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/login/test-common
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/installation
                #:multi-org-feature
                #:installation
                #:*installation*)
  (:import-from #:screenshotbot/model/company
                #:company
                #:prepare-singleton-company
                #:get-singleton-company)
  (:import-from #:screenshotbot/user-api
                #:user
                #:current-company)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/user
                #:user-personal-company
                #:make-user)
  (:import-from #:screenshotbot/login/common
                #:with-current-company)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/login/test-common)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (cl-mock:with-mocks ()
      (&body))))

(test current-company-for-common
  (with-fixture state ()
    (cl-mock:answer
        (auth:session-value :company)
      nil)
    (let ((*installation* (make-instance 'installation)))
      (prepare-singleton-company)
      (is-true (get-singleton-company *installation*))
      (is-true (current-company)))))

(defclass multi-org (multi-org-feature
                     installation)
  ())

(test current-company-for-multi-org
  (with-fixture state ()
    (let* ((*installation* (make-instance 'multi-org))
           (user (make-user))
           (company (user-personal-company user)))
      (cl-mock:answer
          (auth:session-value :company)
        company)
      (is (eql company (current-company :user user)) ))) ())

(test with-current-company
  (with-fixture state ()
    ;; Make sure we don't accidently use a with-fake-request in the
    ;; fixture in the future.
    (is (not (boundp 'hunchentoot:*request*)))
    (let ((company (make-instance 'company)))
      (with-current-company (company)
        (is (eql company (current-company)))))))
