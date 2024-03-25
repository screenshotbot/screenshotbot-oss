;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/model/test-company-graph
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/testing
                #:multi-org-test-installation
                #:with-installation
                #:with-test-user)
  (:import-from #:screenshotbot/model/company-graph
                #:company-graph)
  (:import-from #:fiveam-matchers/lists
                #:has-item)
  (:import-from #:fiveam-matchers/core
                #:is-not
                #:assert-that))
(in-package :screenshotbot/model/test-company-graph)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (&body)))

(test happy-path-graph
  (with-fixture state ()
    (with-installation (:installation (make-instance 'multi-org-test-installation))
     (with-test-user (:user user :company company)
       (assert-that (company-graph company)
                    (has-item user))
       (with-test-user (:user other-user :company company2 :company-name "other company")
         (assert-that (company-graph company)
                      (is-not (has-item other-user))))))))
