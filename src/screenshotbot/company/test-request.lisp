;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/company/test-request
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:screenshotbot/model/company
                #:get-singleton-company
                #:prepare-singleton-company)
  (:import-from #:screenshotbot/company/request
                #:most-recent-company
                #:guess-best-company)
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
                #:signin-get)
  (:import-from #:screenshotbot/model/recorder-run
                #:recorder-run)
  (:import-from #:bknr.datastore
                #:with-transaction)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:util/testing
                #:with-fake-request)
  (:import-from #:fiveam-matchers/core
                #:assert-that)
  (:import-from #:fiveam-matchers/has-length
                #:has-length)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/company/test-request)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation ()
   (with-test-store ()
     (cl-mock:with-mocks ()
       (&body)))))

(test current-company-for-common
  (with-fixture state ()
    (cl-mock:answer
        (auth:session-value :company)
      nil)
    (let ((*installation* (make-instance 'installation)))

      (let* ((company (prepare-singleton-company))
             (user (make-instance 'user)))
        (setf (roles:user-role company user) 'roles:standard-member)
        (assert-that (roles:companies-for-user user)
                     (has-length 1))
        (is-true (guess-best-company nil user))))))

(defclass multi-org (multi-org-feature
                     installation)
  ())

(test current-company-for-multi-org
  (with-fixture state ()
    (let* ((*installation* (make-instance 'multi-org))
           (user (make-user))
           (company (user-personal-company user)))
      (is (eql company (guess-best-company company user)) ))) ())

(test most-recent-company
  (with-fixture state ()
    (let ((company-1 (make-instance 'company)))
      (is (eql nil (most-recent-company (list company-1))))
      (let ((run (make-instance 'recorder-run
                     :screenshot-map nil
                     :company company-1)))
        (is (eql company-1 (most-recent-company (list company-1))))))))
