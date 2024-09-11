;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/github/test-settings
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/github/settings
                #:verified-repo
                #:verified-repo-p)
  (:import-from #:util/store/store
                #:with-test-store)
  (:import-from #:screenshotbot/model/company
                #:company))
(in-package :screenshotbot/github/test-settings)

(util/fiveam:def-suite)

(def-fixture state ()
  (with-test-store ()
    (let ((company (make-instance 'company))
          (company-2 (make-instance 'company)))
      (&body))))

(test verified-repo-p
  (with-fixture state ()
    (make-instance 'verified-repo
                   :company company
                   :repo-id "tdrhq/web")

    ;; I think we want this to be false, but I'm documenting current
    ;; behavior.
    (is-true (verified-repo-p "https://github.com/tdrhq/web" company))

    (is-false (verified-repo-p "https://github.com/tdrhq/web" company-2))))
