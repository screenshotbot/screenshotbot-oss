;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(defpackage :screenshotbot/azure/test-promoter
  (:use #:cl
        #:fiveam
        #:screenshotbot/abstract-pr-promoter)
  (:import-from #:screenshotbot/azure/promoter
                #:parse-org-and-project
                #:azure-promoter)
  (:import-from #:screenshotbot/azure/plugin
                #:azure-git-repo)
  (:import-from #:screenshotbot/git-repo
                #:generic-git-repo))
(in-package :screenshotbot/azure/test-promoter)


(util/fiveam:def-suite)

(def-fixture state ()
  (let ((promoter (make-instance 'azure-promoter)))
    (&body)))

(test valid-repo-for-azure
  (with-fixture state ()
    (is-true (valid-repo? promoter
                         (make-instance 'azure-git-repo
                                        :link "foo")))
    (is-false (valid-repo? promoter
                           (make-instance 'generic-git-repo
                                          :link "foo")))))


(test parse-org-and-project
  (multiple-value-bind
        (org project repo)
      (parse-org-and-project "git@ssh.dev.azure.com:v3/foogroup/Foo%20app/foo-app-flutter" "dev.azure.com")
    (is (equal org "foogroup"))
    (is (equal project "Foo%20app"))
    (is (equal repo "foo-app-flutter"))))
