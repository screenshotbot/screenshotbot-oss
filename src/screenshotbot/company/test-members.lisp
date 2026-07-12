;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/company/test-members
  (:use #:cl
        #:fiveam)
  (:import-from #:it.bese.fiveam
                #:def-fixture)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:only-instance!
                #:with-test-user)
  (:import-from #:screenshotbot/company/members
                #:new-role
                #:role-changed-audit-log
                #:old-role
                #:%set-user-role))
(in-package :screenshotbot/company/test-members)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (with-test-user (:user user
                     :company company
                     :logged-in-p t)
      (&body))))

(test set-user-role
  (with-fixture state ()
    (finishes
      (%set-user-role company user 'roles:admin))
    (let ((log (only-instance! 'role-changed-audit-log)))
      (is (eql 'roles:standard-member (old-role log)))
      (is (eql 'roles:admin (new-role log))))))
