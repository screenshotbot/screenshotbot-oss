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
