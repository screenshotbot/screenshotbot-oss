;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-testing
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/user-api
                #:can-view!
                #:can-view)
  (:import-from #:screenshotbot/testing
                #:multi-org-test-installation
                #:with-installation
                #:with-test-user)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/installation
                #:installation
                #:multi-org-feature)
  (:import-from #:auth/viewer-context
                #:normal-viewer-context))
(in-package :screenshotbot/test-testing)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test ensure-user-can-view-company
  (with-fixture state ()
    (with-test-user (:user user :company company)
      (is-true (auth:can-viewer-view (make-instance 'normal-viewer-context :user user)
                                     company)))
    (with-test-user (:user user :company company)
      (is-true (auth:can-viewer-view (make-instance 'normal-viewer-context :user user)
                                     company)))))

(test ensure-user-can-view-company-multi-org
  (with-installation (:installation (make-instance 'multi-org-test-installation))
    (with-fixture state ()
      (with-test-user (:user user :company company)
        (is-true (auth:can-viewer-view (make-instance 'normal-viewer-context :user user) company))))))

(test with-installation-doesnt-override
  (with-installation (:installation (make-instance 'multi-org-test-installation))
    (with-installation ()
      (is (typep (installation) 'multi-org-test-installation)))))
