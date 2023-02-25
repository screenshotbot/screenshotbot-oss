;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/azure/test-plugin
  (:use #:cl
        #:fiveam)
  (:import-from #:util/store
                #:with-test-store)
  (:import-from #:screenshotbot/azure/plugin
                #:azure-git-repo
                #:azure-settings
                #:azure-plugin)
  (:import-from #:screenshotbot/testing
                #:with-installation)
  (:import-from #:screenshotbot/installation
                #:installation)
  (:import-from #:fiveam-matchers/misc
                #:is-null)
  (:import-from #:fiveam-matchers/core
                #:has-typep
                #:assert-that)
  (:import-from #:screenshotbot/plugin
                #:plugin-parse-repo)
  (:import-from #:screenshotbot/model/company
                #:company))
(in-package :screenshotbot/azure/test-plugin)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-installation (:installation (make-instance
                                     'installation
                                     :plugins (list
                                               (make-instance 'azure-plugin))))
    (with-test-store ()
      (let ((company (make-instance 'company)))
       (&body)))))

(test azure-plugin-happy-path
  (with-fixture state ()
    (is-true (azure-plugin))))

(test plugin-parse-repo-for-non-azure
  (with-fixture state ()
    (assert-that
     (plugin-parse-repo (azure-plugin)
                        company
                        "https://github.com/tdrhq/fast-example")
     (is-null))))

(test plugin-parse-repo-for-azure
  (with-fixture state ()
    (make-instance 'azure-settings
                   :company company
                   :server "https://dev.azure.com"
                   :access-token "foo")
    (assert-that
     (plugin-parse-repo (azure-plugin)
                        company
                        "https://testsbot@dev.azure.com/testsbot/fast-example/_git/fast-example")
     (has-typep 'azure-git-repo))))
