;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/gitlab/test-settings
  (:use #:cl
        #:fiveam
        #:fiveam-matchers)
  (:import-from #:screenshotbot/gitlab/repo
                #:gitlab-repo)
  (:import-from #:screenshotbot/gitlab/plugin
                #:gitlab-plugin)
  (:import-from #:screenshotbot/plugin
                #:plugin-parse-repo)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company)
  (:import-from #:screenshotbot/gitlab/settings
                #:gitlab-settings))
(in-package :screenshotbot/gitlab/test-settings)


(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let* ((company (make-instance 'company))
           (plugin (make-instance 'gitlab-plugin
                                  :access-token "dummy")))
     (&body))))

(test gitlab-plugin-parse-repo
  (with-fixture state ()
    (let ((settings (make-instance 'gitlab-settings
                                   :company company
                                   :url "https://gitlab.com")))
     (assert-that
      (plugin-parse-repo plugin
                         company
                         "https://gitlab.com/tdrhq/fast-example.git")
      (has-typep 'gitlab-repo)))))
